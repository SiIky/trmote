(import
  chicken.process-context
  chicken.string
  chicken.sort
  chicken.port)

(import
  cling
  srfi-1
  typed-records)

(import
  daemon
  transmission
  transmission.utils)

(include "connection-options.scm")

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
    ("status" . ,(sort-by/<predicate 'status 0 (lambda (s1 s2) (not (not (member s2 (cdr (member s1 (*status-order*)))))))))
    ("total-size" . ,(sort-by/<predicate 'totalSize 0 <))

    ; NOTE: Sorted in an ascending manner, i.e., smaller size means higher
    ;       priority, because it's the quickest way to get the greatest number
    ;       of torrents "out of the door".
    ("progress*total-size"
     . ,(lambda (t1 t2)
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
  (define (<*? t1 t2)
    (define (kons <? ret)
      (or ret (<? t1 t2)))
    (fold kons #f sort-by))
  (sort l <*?))

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
                  (update-options ret #:logfile logfile)))
    ))

(define (help*)
  (help *connection-opts*)
  (help *OPTS*))

(define (process-arguments* args)
  (define sort-by
    '(; NOTE: `status`, `priority`, `latest-activity`, and `ratio` basically
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

  (receive (args help?) (update-connection-options! args)
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
          (assert (every identity ret) "Some torrents didn't have the `hashString` field -- this shouldn't have happened!")
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
  ; NOTE: If the call succeeds but we get no torrent or hash, then maybe the
  ;       torrent doesn't exist (anymore?). Let's assume that.
  ; FIXME: If, for example, Transmission goes down, this results in an
  ;        infinite loop until the process is killed or Transmission is
  ;        started up again.
  ; TODO: Try to distinguish failure reasons.
  (and-let* ((torrents (get-torrents* '("status") hash)))
    (every (lambda (torrent)
             (alist-let/nor torrent (status)
                            (not (status/verifying? status))))
           torrents)))

(define (start-verifying torrent)
  (define ((verify* hash))
    (define true (constantly #t))
    (define false (constantly #f))
    (with-transmission-result (torrent-verify #:ids hash) true false))

  (and-let* ((hash (torrent-hash torrent)))
    (or (status/verifying? (torrent-status! hash))
        (try-n 2 (verify* hash)))))

(define (wait-until-verified torrent)
  (and-let* ((hash (torrent-hash torrent)))
    ; TODO: Smarten up this loop (e.g., smarter sleep times).
    (let loop ((sleep-time 10)) ; seconds
      (unless (verified? hash)
        (sleep sleep-time)
        (loop sleep-time)))))

(define ((verify/next sort) verifying verified to-verify include-new?)
  (define (diff to-verify verified)
    (lset-difference
      (lambda (a b) (string=? (torrent-hash a) b))
      to-verify verified))

  (let ((verified (if verifying (cons (torrent-hash verifying) verified) verified))
        (ids (if include-new? #f to-verify)))
    (let ((to-verify (get-torrents ids)))
      (unless to-verify
        (error 'verify/next "Failed to get torrents: " ids))

      (let ((to-verify (filter identity (map torrent-hash (sort (diff to-verify verified))))))
        (values verified to-verify)))))

(define (verify/first sort ids include-new?)
  ((verify/next sort) #f '() (get-torrents/hashes ids) include-new?))

(define (verify/single torrent)
  (let ((hash (torrent-hash torrent)))
    (when (start-verifying hash)
      (wait-until-verified hash))))

(define (verify sort ids include-new?)
  (let ((verify/next (verify/next sort)))
    (receive (verified to-verify) (verify/first sort ids include-new?)
      (let loop ((verified verified)
                 (to-verify to-verify))
        (unless (null? to-verify)
          (let ((verifying (car to-verify))
                (to-verify (cdr to-verify)))
            (verify/single verifying)
            (receive (verified to-verify) (verify/next verifying verified to-verify include-new?)
              (loop verified to-verify))))))))

(define ((verify/action torrent sort))
  (cond
    ((null? torrent) #f)
    ((not torrent) (verify sort #f #t))

    ((null? (cdr torrent))
     (and-let* ((torrent (get-torrents (car torrent))))
       (verify/single torrent)))

    (else (verify sort torrent #f))))

(define (main args)
  (receive (options help?) (process-arguments* args)
    (let ((torrent (options-torrent options))
          (sort (sort/by (options-sort-by options)))
          (daemon? (options-daemon options))
          (logfile (options-logfile options)))
      (let ((action (verify/action torrent sort))
            (torrent (and torrent (get-torrents/hashes torrent))))
        (cond
          (help? (help*))
          ((null? torrent) #f) ; noop

          (daemon?
            (let ((pid (daemon action
                               #:cwd #f ; Don't change CWD
                               #:killothers? #t
                               #:stderr logfile
                               #:stdout logfile
                               #:want-pid? #t)))
              (unless pid (error 'main "Failed to start daemon"))
              (print pid)))

          (else (action)))))))

(main (command-line-arguments))
