(import
  chicken.process-context
  chicken.string
  chicken.port)

(import
  cling
  srfi-1
  typed-records)

(import
  transmission
  transmission.utils)

(include "connection-options.scm")

; TODO: Order by status as well.
(defstruct options
           sort-by
           torrent

           daemon
           logfile

           rest)

(define ((sort-by/<predicate key default <?) t1 t2)
  (let ((atr1 (alist-ref key t1 eq? default))
        (atr2 (alist-ref key t2 eq? default)))
    (<? atr1 atr2)))

(define *sort-by/<predicates*
  `(
    ("id" . ,(sort-by/<predicate 'id 0 <))
    ("progress" . ,(sort-by/<predicate 'percentDone 0 <))
    ("ratio" . ,(sort-by/<predicate 'uploadRatio 0 <))
    ("total-size" . ,(sort-by/<predicate 'totalSize 0 <))
    ))

(define *sort-by/alternatives* (map car *sort-by/<predicates*))

(define (sort-by-str->sort-by-pred s)
  (let ((pred (alist-ref s *sort-by/<predicates* string=? #f)))
    (unless pred
      (error 'sort-by-str->sort-by-pred "`sort-by` predicate wasn't found -- this shouldn't have happened!"))
    pred))

(define (sort-by-str-list->sort-by-pred-list l)
  (map sort-by-str->sort-by-pred l))

(define ((sort/by sort-by) l)
  (define (<? t1 t2)
    (fold-left (lambda (<? ret) (or ret (<? t1 t2))) sort-by))
  (sort l <?))

(define *OPTS*
  (cling
    (lambda (ret . rest)
      (update-options ret #:rest rest))

    (arg '((--sort-by) . sort-by)
         #:help "The order in which the torrents should be verified."
         #:kons (lambda (ret _ sort-by)
                  (let* ((sort-by (string-split sort-by "," #t))
                         (invalid (filter (complement (cute member <> *sort-by/alternatives* string=?)) sort-by)))
                    (cond
                      ((null? sort-by)
                       (error '--sort-by "`sort-key` must be in " *sort-by/alternatives*))
                      ((null? invalid)
                       (update-options ret #:sort-by (sort-by-str-list->sort-by-pred-list sort-by)))
                      (else
                        (error '--sort-by "The following `sort-by` keys are not valid: " invalid ". `sort-by` must be in " *sort-by/alternatives*))))))

    ; TODO: Parse the given string.
    (arg '((--torrent -t) . torrent)
         #:help "The torrents to verify (all by default)."
         #:kons (lambda (ret _ torrent)
                  (update-options ret #:torrent torrent)))


    (arg '((--daemon))
         #:help "Run in the background."
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
                                 #:sort-by (sort-by-str-list->sort-by-pred-list '("total-size" "ratio"))
                                 #:logfile #t
                                 )
                               args)
            help?)))

(define (try-n tries proc #!optional (succ? identity))
  (assert (positive? tries))
  (let loop ((n 0))
    (if (< n tries)
        (let ((res (proc)))
          (if (succ? res)
              res
              (loop (add1 n))))
        #f)))

(define (torrent-ref torrent key #!optional default)
  (if torrent
      (alist-ref key torrent eq? default)
      default))

(define (torrent-hash torrent)
  (if (string? torrent)
      torrent
      (torrent-ref torrent 'hashString #f)))

(define (get-torrents* fields #!optional ids)
  (with-transmission-result (torrent-get fields #:ids ids)
                            (lambda (r . _) (alist-ref 'torrents r eq? '()))
                            (constantly #f)))

(define (get-torrents #!optional ids)
  (get-torrents* '("hashString" "id" "percentDone" "totalSize") ids))

(define (get-torrents/hashes #!optional ids)
  (let ((ret (get-torrents* '("hashString") ids)))
    (if ret
        (let ((ret (map torrent-hash ret)))
          (assert (every identity ret) "Some torrents didn't have the `hashString` field -- this shouldn't happen!")
          ret)
        '())))

; NOTE: This procedure doesn't actually check that a torrent has been verified,
;       but that it's not being checked or queued to be checked. Therefore, it
;       assumes the torrent was put on queue to be verified, i.e.,
;       `torrent-verify` was called for it.
(define (verified? hash)
  (with-transmission-result
    (torrent-get '("status") #:ids hash)
    ; NOTE: If the call succeeds but we get no torrent or hash, then maybe the
    ;       torrent doesn't exist (anymore?). Let's assume that.
    (lambda (r . _)
      (alist-let/or r (torrents)
                    (alist-let/or torrents (status)
                                  (not (or (= status status/check-wait)
                                           (= status status/check))))))
    ; TODO: Try to distinguish failure reasons.
    (constantly #f)))

(define (start-verifying torrent)
  (and-let* ((hash (torrent-hash torrent)))
    (try-n 2 (cut with-transmission-result (torrent-verify hash) (constantly #t) (constantly #f)))))

(define (wait-until-verified torrent)
  (and-let* ((hash (torrent-hash torrent)))
    ; TODO: Smarten up this loop (e.g., smarter sleep times).
    (let loop ((sleep-time 10)) ; seconds
      (unless (verified? hash)
        (sleep sleep-time)
        (loop sleep-time)))))

(define ((verify/next sort) verifying verified to-verify include-new?)
  (let ((verified (if verifying (cons verifying verified) verified))
        (ids (if include-new?
                 #f
                 (filter identity (map torrent-hash to-verify)))))
    (let ((to-verify (get-torrents ids)))
      (unless to-verify
        (error 'verify/next "Failed to get torrents: " ids))

      (let ((to-verify (sort (filter (lambda (t)
                                       (member (torrent-hash t)
                                               verified
                                               (lambda (h t)
                                                 (string=? h (torrent-hash t)))))
                                     to-verify))))
        (if (null? to-verify)
            (values #f verified '())
            (values (car to-verify) verified (cdr to-verify)))))))

(define (verify/single torrent)
  (let ((hash (torrent-hash torrent)))
    (when (start-verifying hash)
      (wait-until-verified hash))))

(define (verify sort ids include-new?)
  (let ((verify/next (verify/next sort)))
    (let-values (((verifying verified to-verify)
                  (if ids
                      (verify/next #f '() (get-torrents ids) #f)
                      (verify/next #f '() '()                #t))))
      (let loop ((verifying verifying)
                 (verified verified)
                 (to-verify to-verify))
        (verify/single verifying)
        (let-values (((verifying verified to-verify) (verify/next verifying verified to-verify include-new?)))
          (or (null? to-verify)
              (loop verifying verified to-verify)))))))

(define (verify/action torrent sort)
  (cond
    ((null? torrent)       (constantly #f))
    ((not torrent)         (cute verify sort #f #t))
    ((null? (cdr torrent)) (cute verify/single (car torrent)))
    (else                  (cute verify sort torrent #f))))

(define (main args)
  (let-values (((options help?) (process-arguments* args)))
    (let ((torrent (options-torrent options))
          (sort (sort/by (options-sort-by options)))
          (daemon (options-daemon options))
          (logfile (options-logfile options)))
      (let ((action (verify/action torrent sort))
            (torrent (and torrent (get-torrents/hashes torrent))))
        (cond
          (help? (help*))
          ((null? torrent) #f) ; noop
          (daemon
            (let ((pid (daemon action
                               #:stderr logfile
                               #:stdout logfile
                               #:want-pid? #t
                               #:killothers? #t)))
              (unless pid
                (error 'main "Failed to start daemon"))
              (eprint "Daemon started with PID " pid)
              (print pid)))
          (else (action)))))))

(main (command-line-arguments))
