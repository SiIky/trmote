;;; TODO:
;;; - [ ] Add some way to exclude auto updates (maybe with labels)
(import
  chicken.process-context
  chicken.string
  chicken.port)

(import
  ; NOTE: Not an egg yet. Can be found here: https://github.com/siiky/cling
  cling
  ; NOTE: Not an egg yet. Can be found here: https://github.com/siiky/daemon
  daemon
  srfi-1
  srfi-42
  transmission
  transmission.utils
  typed-records)

(include "connection-options.scm")

(define-syntax loop
  (syntax-rules ()
    ((loop body ...)
     (let loop ()
       body
       ...
       (loop)))))

(defstruct options
           high-priority-ratio
           low-priority-ratio

           daemon
           every
           logfile

           help
           rest
           )

(define *OPTS*
  (cling
    (lambda (ret . rest)
      (update-options ret #:rest rest))

    (arg '((-h --help))
         #:help "Show this help text"
         #:kons (lambda (ret _ _) (update-options ret #:help #t)))


    (arg '((--high-priority-ratio) . ratio)
         #:help "Torrents with ratio < this are considered high priority."
         #:kons (lambda (ret _ ratio)
                  (let ((ratio (string->number ratio)))
                    (assert (number? ratio))
                    (update-options ret #:high-priority-ratio ratio))))

    (arg '((--low-priority-ratio) . ratio)
         #:help "Torrents with ratio >= this are considered low priority."
         #:kons (lambda (ret _ ratio)
                  (let ((ratio (string->number ratio)))
                    (assert (number? ratio))
                    (update-options ret #:low-priority-ratio ratio))))


    ; NOTE: The CSRF token is renewed every hour.
    ; @see https://github.com/transmission/transmission/blob/cfce6e2e3a9b9d31a9dafedd0bdc8bf2cdb6e876/libtransmission/session-id.c#L27
    (arg '((--every) . every)
         #:help "Update priorities every EVERY seconds."
         #:kons (lambda (ret _ every)
                  (let ((every (string->number every)))
                    (assert every)
                    (assert (positive? every))
                    (update-options ret #:every every))))

    (arg '((--daemon))
         #:help "Combined with --every, update periodically in the background."
         #:kons (lambda (ret _ _)
                  (update-options ret #:daemon #t)))

    (arg '((--logfile) . logfile)
         #:help "Combined with --daemon, save program output to LOGFILE"
         #:kons (lambda (ret _ logfile)
                  (update-options ret #:logfile (normalize-pathname (make-absolute-pathname (current-directory) logfile)))))
    ))

(define (help*)
  (help *connection-opts*)
  (help *OPTS*))

(define (process-arguments* #!optional (args (command-line-arguments)))
  (process-arguments *OPTS*
                     (make-options #:low-priority-ratio 4 #:high-priority-ratio 2)
                     (set-connection-options! args)))

(define ((alist? key-pred? #!optional (value-predicate? (constantly #t))) lst)
  (and (list? lst)
       (every (lambda (elem)
                (and (pair? elem)
                     (key-pred? (car elem))
                     (value-predicate? (cdr elem))))
              lst)))

(define (group-by groups lst)
  (define (kons elem ret)
    (let ((group-key (member elem groups
                             (lambda (elem group-key/group-pred?)
                               (let ((group-key (car group-key/group-pred?))
                                     (group-pred? (cdr group-key/group-pred?)))
                                 (group-pred? elem))))))
      (assert group-key)
      (let* ((group-key (caar group-key))
             (group-elems (alist-ref group-key ret)))
        (assert group-elems)
        (let ((group-elems (cons elem group-elems)))
          (alist-update group-key group-elems ret)))))

  (define knil (map (lambda (kv) (cons (car kv) '())) groups))

  (assert ((alist? symbol? procedure?) groups))
  (fold kons knil lst))

(define ((*-priority? pred?) torrent)
  (alist-let/and torrent (uploadRatio)
                 (pred? uploadRatio)))

(define (low-priority? low) (*-priority? (cute >= <> low)))
(define (normal-priority? low high) (*-priority? (lambda (ratio) (and (<= ratio low) (>= ratio high)))))
(define (high-priority? high) (*-priority? (cute < <> high)))

(define ((transmission-update-seed-priority* priority) tors)
  (let ((hashes (list-ec (:list tor tors)
                         (:let bandwidthPriority (alist-ref 'bandwidthPriority tor))
                         (:let hashString (alist-ref 'hashString tor))
                         (:let status (alist-ref 'status tor))
                         (not (= bandwidthPriority priority))
                         (if (or (= status status/seed-wait) (= status status/seed)))
                         hashString)))
    (unless (null? hashes)
      (with-transmission-result (torrent-set #:ids hashes #:bandwidth-priority priority)
                                (constantly #t)
                                (lambda (result . _) (eprint "Failed to update priority: " result))))))

(define transmission-update-seed-priority/low (transmission-update-seed-priority* priority/low))
(define transmission-update-seed-priority/normal (transmission-update-seed-priority* priority/normal))
(define transmission-update-seed-priority/high (transmission-update-seed-priority* priority/high))

(define (update-priorities low high)
  (with-transmission-result (torrent-get '("bandwidthPriority" "hashString" "status" "uploadRatio") #:ids #f)
                            (lambda (arguments . _)
                              (alist-let/and arguments (torrents)
                                             (alist-let/and (group-by `((high . ,(high-priority? high))
                                                                        (normal . ,(normal-priority? low high))
                                                                        (low . ,(low-priority? low)))
                                                                      (vector->list torrents))
                                                            (low normal high)
                                                            (transmission-update-seed-priority/low low)
                                                            (transmission-update-seed-priority/normal normal)
                                                            (transmission-update-seed-priority/high high))))
                            (lambda (result . _)
                              (eprint "Failed to get torrents: " result))))

(define (update-priorities/every low high every)
  (loop
    (update-priorities low high)
    (sleep every)))

(define (update-priorities/daemon low high every logfile)
  (let ((logfile (or logfile #t))
        (every (or every (* 10 60))))
    (daemon (cute update-priorities/every low high every)
            #:stderr logfile
            #:stdout logfile
            #:want-pid? #t
            #:killothers? #t)))

(define (main args)
  (let ((options (process-arguments* args)))
    (let ((low (options-low-priority-ratio options))
          (high (options-high-priority-ratio options))
          (every (options-every options))
          (logfile (options-logfile options)))
      (cond
        ((options-help options)
         (help*))

        ((options-daemon options)
         (let ((pid (update-priorities/daemon low high every logfile)))
           (unless pid
             (eprint "Failed to start daemon")
             (exit 1))
           (eprint "Daemon started -- PID " pid)
           (print pid)))

        (every
          (update-priorities/every low high every))

        (else
          (update-priorities low high))))))

(main (command-line-arguments))
