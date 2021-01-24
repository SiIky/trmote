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

(define *status-order*
  (make-parameter
    `(
      ; NOTE: `check` & `check-wait` first to try to avoid rechecking -- only
      ;        useful in case another client is being used concurrently.
      ,status/check
      ,status/check-wait
      ; NOTE: If we're asking to verify torrents, it's probably because we
      ;       suspect they might be corrupt. This status is the one in which a
      ;       peer can suffer most from corrupt files, because if corrupted
      ;       pieces are being sent to other peers, they'll eventually stop
      ;       asking us for anything. The faster we can get seeding torrents
      ;       checked, the faster they can resume seeding.
      ,status/seed
      ,status/download
      ,status/seed-wait
      ,status/download-wait
      ; NOTE: Least priority, because we clearly don't care about a stopped
      ;       torrent's files' integrity.
      ,status/stopped
      )))

(define ((sort-by/<predicate key default <?) t1 t2)
  (let ((atr1 (alist-ref key t1 eq? default))
        (atr2 (alist-ref key t2 eq? default)))
    (<? atr1 atr2)))

(define *sort-by/<predicates*
  `(
    ("id" . ,(sort-by/<predicate 'id 0 <))
    ("latest-activity" . ,(sort-by/<predicate 'activityDate 0 >))
    ("priority" . ,(sort-by/<predicate 'bandwidthPriority priority/low >))
    ("progress" . ,(sort-by/<predicate 'percentDone 0 >))
    ("queue-position" . ,(sort-by/<predicate 'queuePosition 0 <))
    ; NOTE: Sorted in an ascending manner, i.e., lesser ratio means higher
    ;       priority, because it needs to be seeded more than a torrent with
    ;       greater ratio.
    ("ratio" . ,(sort-by/<predicate 'uploadRatio 0 <))
    ("status" . ,(sort-by/<predicate 'status 0 (lambda (s1 s2) (member s2 (cdr (member s1 (*status-order*)))))))
    ("total-size" . ,(sort-by/<predicate 'totalSize 0 <))

    ; NOTE: Sorted in an ascending manner, i.e., smaller size means higher
    ;       priority, because it's the quickest way to get the greatest number
    ;       of torrents "out of the door".
    ("progress*total-size"
     . (lambda (t1 t2)
         (let ((p1 (alist-ref 'progress t1 eq? 0))
               (ts1 (alist-ref 'totalSize t1 eq? 0))
               (p2 (alist-ref 'progress t2 eq? 0))
               (ts2 (alist-ref 'totalSize t2 eq? 0)))
           (< (* p1 ts1) (* p2 ts2)))))
    ))

(define (status-str->status-int str)
  (let ((rel `(("stopped"       . ,status/stopped)
               ("check-wait"    . ,status/check-wait)
               ("check"         . ,status/check)
               ("download-wait" . ,status/download-wait)
               ("download"      . ,status/download)
               ("seed-wait"     . ,status/seed-wait)
               ("seed"          . ,status/seed))))
    (alist-ref str rel string=? #f)))

(define (status-str-list->status-int-list l)
  (map status-str->status-int l))

(define *sort-by/alternatives* (map car *sort-by/<predicates*))

(define (sort-by-str->sort-by-pred s)
  (let ((pred (alist-ref s *sort-by/<predicates* string=? #f)))
    (unless pred
      (error 'sort-by-str->sort-by-pred "`sort-by` predicate wasn't found -- this shouldn't have happened!"))
    pred))

(define (sort-by-str-list->sort-by-pred-list l)
  (map sort-by-str->sort-by-pred l))

(define ((sort/by sort-by) l)
  (let* ((<? (lambda (t1 t2)
               (fold-left (lambda (<? ret) (or ret (<? t1 t2))) sort-by))))
    (sort l <?)))

(define (status/verifying? status)
  (or (= status status/check-wait)
      (= status status/check)))

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
                       (error '--sort-by "`sort-key` must not be empty."))
                      ((null? invalid)
                       (update-options ret #:sort-by (sort-by-str-list->sort-by-pred-list sort-by)))
                      (else
                        (error '--sort-by "The following `sort-by` keys are not valid: " invalid ". `sort-by` must be in " *sort-by/alternatives*))))))

    ; TODO: Parse the given string.
    (arg '((--status-order) . status-order)
         #:help "The order to sort statuses by. Only useful if `status` is specified with `--sort-by`."
         #:kons (lambda (ret _ status-order)
                  (eprint "`--status-order` isn't implemented yet -- ignoring and using the default order of " (*status-order*))
                  (update-options ret #:status-order (*status-order*))))

    ; TODO: Parse the given string.
    (arg '((--torrent -t) . torrent)
         #:help "The torrents to verify (all by default)."
         #:kons (lambda (ret _ torrent)
                  (eprint "`--torrent` isn't implemented yet -- ignoring and acting on all torrents.")
                  (update-options ret #:torrent #f)))


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
  (define sort-by
    '(
      ; NOTE: `status`, `priority`, `latest-activity`, and `ratio` basically
      ;       all give order based on a torrent's priority, while
      ;       `progress*total-size` gives order based on how quickly a torrent
      ;       can be verified, i.e., smaller torrents can be verified in less
      ;       time than larger torrents.
      "status"
      "priority"
      "latest-activity"
      "ratio"
      "progress*total-size"
      ))

  (let-values (((args help?) (update-connection-options! args)))
    (values (process-arguments *OPTS*
                               (make-options
                                 #:sort-by (sort-by-str-list->sort-by-pred-list sort-by)
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
                            (lambda (r . _) (vector->list (alist-ref 'torrents r eq? #())))
                            (constantly #f)))

(define (get-torrents #!optional ids)
  (let ((fields
          '(
            "activityDate"
            "bandwidthPriority"
            "hashString"
            "id"
            "percentDone"
            "queuePosition"
            "status"
            "totalSize"
            )))
    (get-torrents* fields ids)))

(define (get-torrents/hashes #!optional ids)
  (let ((ret (get-torrents* '("hashString") ids)))
    (if ret
        (let ((ret (map torrent-hash ret)))
          (assert (every identity ret) "Some torrents didn't have the `hashString` field -- this shouldn't happen!")
          ret)
        '())))

(define (torrent-status torrent #!optional (default status/seed-wait))
  (if torrent
      (torrent-ref torrent 'status default)
      default))

(define (torrent-status! torrent)
  (let ((r (get-torrents* '("status") torrent)))
    (and r
         (torrent-status (car r)))))

; NOTE: This procedure doesn't actually check that a torrent has been verified,
;       but that it's not being checked nor queued to be checked. Therefore, it
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
    ; FIXME: If, for example, Transmission goes down, this results in an
    ;        infinite loop until the process is killed or Transmission is
    ;        started up again.
    ; TODO: Try to distinguish failure reasons.
    (constantly #f)))

(define (start-verifying torrent)
  (and-let* ((hash (torrent-hash torrent)))
    (or (status/verifying? (torrent-status! hash))
        (try-n 2 (cut with-transmission-result (torrent-verify hash) (constantly #t) (constantly #f))))))

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
