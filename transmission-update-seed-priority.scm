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
  skip-labels

  every
  every?
  daemon
  logfile

  rest
  )

(define (=*> val funs)
  (foldl (lambda (val fun) (fun val)) val funs))

(define ((-*> funs) val)
  (=*> val funs))

(define (=> val . funs)
  (=*> val funs))

(define ((-> . funs) val)
  (=*> val funs))

(define *OPTS*
  (cling
    (lambda (ret . rest)
      (update-options ret #:rest rest))

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

    (arg '((--skip-label) . label)
         #:help "Don't change priority of torrents that have the given label."
         #:kons (lambda (ret _ label)
                  (update-options ret #:skip-labels (cons label (options-skip-labels ret)))))


    ; NOTE: The CSRF token is renewed every hour.
    ; @see https://github.com/transmission/transmission/blob/cfce6e2e3a9b9d31a9dafedd0bdc8bf2cdb6e876/libtransmission/session-id.c#L27
    (arg '((--every) . every)
         #:help "Update priorities every EVERY seconds."
         #:kons (lambda (ret _ every)
                  (let ((every (string->number every)))
                    (assert every)
                    (assert (positive? every))
                    (update-options ret #:every every #:every? #t))))

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

(define (process-arguments* args)
  (let-values (((args help?) (update-connection-options! args)))
    (values (process-arguments *OPTS*
                               (make-options
                                 #:low-priority-ratio 4
                                 #:high-priority-ratio 2
                                 #:skip-labels '()
                                 #:every (* 10 60)
                                 #:logfile #t
                                 )
                               args)
            help?)))

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
             (group-elems (reply-ref group-key ret)))
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
                         (:let bandwidthPriority (reply-ref 'bandwidthPriority tor))
                         (:let hashString (reply-ref 'hashString tor))
                         (:let status (reply-ref 'status tor))
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

(define (update-priorities low high skip-labels)
  (define !skip?
    (-> (cute reply-ref 'labels <> eq? #())
        vector->list
        (cute lset-intersection string=? skip-labels <>)
        null?))

  (with-transmission-result (torrent-get '("bandwidthPriority" "hashString" "status" "uploadRatio" "labels") #:ids #f)
                            (lambda (arguments . _)
                              (alist-let/and arguments (torrents)
                                             (let ((torrents (=> torrents
                                                                 vector->list
                                                                 (cute filter !skip? <>)
                                                                 (cute group-by `((high . ,(high-priority? high))
                                                                                  (normal . ,(normal-priority? low high))
                                                                                  (low . ,(low-priority? low)))
                                                                       <>))))
                                               (alist-let/and torrents (low normal high)
                                                              (transmission-update-seed-priority/low low)
                                                              (transmission-update-seed-priority/normal normal)
                                                              (transmission-update-seed-priority/high high)))))
                            (lambda (result . _)
                              (eprint "Failed to get torrents: " result))))

(define (update-priorities/every low high skip-labels every)
  (loop
    (update-priorities low high skip-labels)
    (sleep every)))

(define (update-priorities/daemon low high skip-labels every logfile)
  (daemon (cute update-priorities/every low high skip-labels every)
          #:stderr logfile
          #:stdout logfile
          #:want-pid? #t
          #:killothers? #t))

(define (main args)
  (let-values (((options help?) (process-arguments* args)))
    (let ((low (options-low-priority-ratio options))
          (high (options-high-priority-ratio options))
          (skip-labels (options-skip-labels options))
          (every (options-every options))
          (every? (options-every? options))
          (daemon? (options-daemon options))
          (logfile (options-logfile options)))
      (cond
        (help?  (help*))

        (every?
          (if daemon?
            (let ((pid (update-priorities/daemon low high skip-labels every logfile)))
              (unless pid
                (error 'main "Failed to start daemon"))
              (eprint "Daemon started with PID " pid)
              (print pid))
            (update-priorities/every low high skip-labels every)))

        (else (update-priorities low high skip-labels))))))

(main (command-line-arguments))
