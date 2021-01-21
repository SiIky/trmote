(import
  chicken.port
  chicken.process-context
  chicken.sort
  chicken.string)

(import
  cling
  srfi-1
  srfi-42
  transmission
  transmission.utils
  typed-records)

(define-syntax eprint
  (syntax-rules ()
    ((eprint arg ...)
     (with-output-to-port (current-error-port) (lambda () (print arg ...))))))

(defstruct options
           host
           port
           authenv
           username
           password

           download-dirs
           help
           ratio<=
           ratio>=
           rest
           statuses
           )

(define *OPTS*
  (cling
    (lambda (ret _ rest) (update-options ret #:rest rest))

    (arg '((-h --help))
         #:help "Show this help text"
         #:kons (lambda (ret _ _)
                  (update-options ret #:help #t)))


    (arg '((--host) . host)
         #:help "The host of the transmission instance"
         #:kons (lambda (ret _ host)
                  (update-options ret #:host host)))

    (arg '((--port) . port)
         #:help "The port of the transmission instance"
         #:kons (lambda (ret _ port)
                  (update-options ret #:port port)))

    (arg '((--username) . username)
         #:help "The username to access the transmission instance"
         #:kons (lambda (ret _ username)
                  (update-options ret #:username username)))

    (arg '((--password) . password)
         #:help "The password to access the transmission instance"
         #:kons (lambda (ret _ password)
                  (update-options ret #:password password)))

    (arg '((--authenv -ne))
         #:help "Like transmission-remote, use the environment variable TR_AUTH"
         #:kons (lambda (ret _ _)
                  (update-options ret #:authenv #t)))


    (arg '((--download-dir) . download-dir)
         #:help "What directories to filter for"
         #:kons (lambda (ret _ download-dir)
                  (update-options ret #:download-dirs (lset-adjoin string=? (options-download-dirs ret) download-dir))))

    (arg '((--ratio-ge) . ratio)
         #:help "Filter for torrents with a ratio >= than the given argument"
         #:kons (lambda (ret _ ratio)
                  (update-options ret #:ratio>= (string->number ratio))))

    (arg '((--ratio-le) . ratio)
         #:help "Filter for torrents with a ratio <= than the given argument"
         #:kons (lambda (ret _ ratio)
                  (update-options ret #:ratio<= (string->number ratio))))

    (arg '((--status) . status)
         #:help "Filter for torrents in this status"
         #:kons (lambda (ret _ status)
                  (update-options ret #:statuses (lset-adjoin string=? (options-statuses ret) status))))
    ))

(define (set-parameters! options)
  (let ((host (options-host options))
        (port (options-port options))
        (authenv (options-authenv options))
        (username (options-username options))
        (password (options-password options)))

    (when host (*host* host))
    (when port (*port* port))

    ; Update authentication variables
    (cond
      (authenv
        (let ((tr_auth (get-environment-variable "TR_AUTH")))
          (unless tr_auth
            (eprint "TR_AUTH not set"))
          (let ((username/password (string-split tr_auth ":")))
            (unless (= (length username/password) 2)
              (eprint "Splitting TR_AUTH by ':' didn't result in 2 elements; authentication will most likely fail."))
            (let ((username (car username/password))
                  (password (cadr username/password)))
              (*username* username)
              (*password* password)))))

      ((or username password)
       (unless (and username password)
         (eprint "Did you forget to set both username and password? Authentication may fail."))
       (when username (*username* username))
       (when password (*password* password))))
    #t))

(define (status-string->status-constant str)
  (alist-ref
    str
    `(("check" . ,status/check)
      ("check-wait" . ,status/check-wait)
      ("download" . ,status/download)
      ("download-wait" . ,status/download-wait)
      ("seed" . ,status/seed)
      ("seed-wait" . ,status/seed-wait)
      ("stopped" . ,status/stopped))
    string=? #f))

(define status-constant->status-string
  (let ((vec #("stopped" "check-wait" "check" "download-wait" "download" "seed-wait" "seed")))
    (lambda (status)
      (vector-ref vec status))))

(define (main args)
  (let ((options (process-arguments
                   *OPTS*
                   (make-options
                     #:host #f
                     #:port #f
                     #:authenv #f
                     #:username #f
                     #:password #f

                     #:download-dirs '()
                     #:help #f
                     #:ratio<= #f
                     #:ratio>= #f
                     #:rest #f
                     #:statuses '()
                     )
                   args))
        (true (constantly #t)))

    (let ((want-dir?
            (let ((wanted-dirs (options-download-dirs options)))
              (if (null? wanted-dirs)
                  true
                  (cute member <> wanted-dirs))))

          (ratio>=?
            (let ((ratio>= (options-ratio>= options)))
              (if ratio>=
                  (cute >= <> ratio>=)
                  true)))

          (ratio<=?
            (let ((ratio<= (options-ratio<= options)))
              (if ratio<=
                  (cute <= <> ratio<=)
                  true)))

          (want-status?
            (let* ((wanted-statuses (options-statuses options))
                   (original-len (length wanted-statuses))
                   (wanted-statuses (filter identity (map status-string->status-constant wanted-statuses)))
                   (final-len (length wanted-statuses)))
              (unless (= original-len final-len)
                (eprint "Some status strings weren't valid and couldn't be converted, so they've been ignored. Using these statuses: " wanted-statuses))

              (if (null? wanted-statuses)
                  true
                  (o not not (cute member <> wanted-statuses))))))

      (if (options-help options)
          (help *OPTS*)
          (begin
            (set-parameters! options)
            (with-transmission-result (torrent-get '("hashString" "downloadDir" "status" "name" "uploadRatio") #:ids #f)
                                      (lambda (arguments . _)
                                        (alist-let/and arguments (torrents)
                                                       (assert torrents)
                                                       (let ((tors (list-ec (:vector tor torrents)
                                                                            (:let ratio (reply-ref 'uploadRatio tor))
                                                                            (:let download-dir (reply-ref 'downloadDir tor))
                                                                            (:let status (reply-ref 'status tor))

                                                                            (and (ratio<=? ratio)
                                                                                 (ratio>=? ratio)
                                                                                 (want-status? status)
                                                                                 (want-dir? download-dir))

                                                                            (alist-keep-keys tor 'hashString 'name 'uploadRatio 'status))))
                                                         (for-each (lambda (torrent)
                                                                     (alist-let/and torrent ((hash-string hashString) name (upload-ratio uploadRatio) status)
                                                                                    (print hash-string #\tab (status-constant->status-string status) #\tab upload-ratio #\tab name)))
                                                                   (sort tors (lambda (tor1 tor2)
                                                                                (>= (reply-ref 'uploadRatio tor1)
                                                                                    (reply-ref 'uploadRatio tor2))))))))))))))

(main (command-line-arguments))
