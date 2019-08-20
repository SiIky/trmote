(import
  scheme
  (only chicken.base
        compose
        cut
        exit
        print)
  (only chicken.irregex
        irregex-match?
        sre->irregex)
  (only chicken.pretty-print
        pp)
  (only chicken.process-context
        command-line-arguments
        program-name)
  (only chicken.string
        string-split))

(import
  (only cling
        *program-name*
        *usage*
        arg
        cling
        help
        process-arguments)
  (only defstruct defstruct)
  (only srfi-1
        append-map
        every
        iota)
  (only transmission
        *host*
        *password*
        *port*
        *session-id*
        *url*
        *username*
        torrent-get))

(defstruct options actions errors help torrent)

(define (string->torrents-list str)
  (define valid-torrent-list?
    (let* ((sha1-re   '(= 40 xdigit))
           (number-re '(+ num))
           (number-re `(seq ,number-re (* (seq "-" ,number-re))))
           (tor-re    `(or ,sha1-re ,number-re))
           (re        `(seq ,tor-re (* (seq "," ,tor-re))))
           (re        `(or "all" "active" ,re))
           (re         (sre->irregex re)))
      (lambda (str)
        (irregex-match? re str))))

  (define (id-int ret)
    (define (single? l) (and (pair? l) (null? (cdr l))))
    (define (double? l) (and (pair? l) (single? (cdr l))))
    (define (enum-from-to from to) (iota (add1 (- to from)) from))
    (let* ((ret (string-split ret ","))
           (ret (map (cut string-split <> "-") ret))
           (ret
             (map
               (lambda (int)
                 (cond
                   ((single? int)
                    (if (= (string-length (car int)) 40) ; SHA-1 ?
                        int
                        `(,(string->number (car int))))) ; Assume its an ID otherwise
                   ((double? int)
                    (let ((fst (string->number (car int)))
                          (snd (string->number (cadr int))))
                      (if (and fst snd (<= fst snd))
                          `(,fst ,snd)
                          `(#f))))))
               ret))
           (ret
             (append-map
               (lambda (int)
                 (cond ((single? int) int)
                       ((double? int) (enum-from-to (car int) (cadr int)))))
               ret)))
      (and (every identity ret) ret)))

  (if (valid-torrent-list? str)
      (cond
        ((string=? str "all") '(#t . #f))
        ((string=? str "active") '(#t . "recently-active"))
        (else (let ((res (id-int str))) `(,(not (not res)) . ,res))))
      '(#f . #f)))

(define (!f? x f) (if x (f x) x))

(define (show-results results show)
  (let ((results (vector->list results)))
    (let ((arguments (!f? (assoc "arguments" results) (compose vector->list cdr)))
          (result (!f? (assoc "result" results) cdr)))
      (if (and result (string=? result "success"))
          (show arguments)
          (print "The server replied with: " result)))))

(define (errors-show errors) (for-each force errors) (exit (length errors)))
(define (options-add-error-message options . rest)
  (update-options options #:errors `(,(delay (apply print rest)) . ,(options-errors options))))

(define (actions-run actions) (for-each force actions))
(define (options-add-action make-act)
  (lambda (options switch args)
    (update-options options #:actions `(,(make-act options switch args) . ,(options-actions options)))))

(define (action/list options switch args)
  (delay
    (let ((ids (options-torrent options))
          (fields '("id" "name")))
      (let ((res (torrent-get fields #:ids ids)))
        (show-results res pp)))))

(define (action/remove options switch args)
  (delay (print "Removing torrents: " (options-torrent options))))
(define (action/remove-and-delete options switch args)
  (delay (print "Removing and deleting torrents: " (options-torrent options))))

(define *OPTS*
  (cling
    (lambda (ret _ rest)
      (if (null? rest) ret
          (options-add-error-message
            ret "The following arguments will be ignored -- " rest)))

    (arg '((-? -h --help))
         #:help "Show this help message"
         #:kons (lambda (ret _ _) (update-options ret #:help #t)))

    (arg '((--host) . host)
         #:help "Hostname of the Transmission daemon"
         #:kons (lambda (ret _ host) (*host* host) ret))

    (arg '((--port) . port)
         #:help "The RPC server port" #:kons
         (lambda (ret _ port)
           (let ((port-n (string->number port)))
             (if port-n
                 (begin (*port* port-n) ret)
                 (options-add-error-message ret "Not a valid port: " port)))))

    (arg '((--url) . url)
         #:help "The RPC server path"
         #:kons (lambda (ret _ url) (*url* url) ret))

    (arg '((--username) . username)
         #:help "The RPC username"
         #:kons (lambda (ret _ username) (*username* username) ret))

    (arg '((--password) . password)
         #:help "The RPC password"
         #:kons (lambda (ret _ password) (*password* password) ret))

    (arg '((-t --torrent) . torrent)
         #:help "The target torrents" #:kons
         (lambda (ret _ torrent)
           (let ((res (string->torrents-list torrent)))
             (if (car res)
                 (update-options ret #:torrent (cdr res))
                 (options-add-error-message ret "Not a valid torrents list: " torrent)))))

    (arg '((-l --list))
         #:help "List torrents"
         #:kons (options-add-action action/list))

    (arg '((-r --remove))
         #:help "Remove torrents"
         #:kons (options-add-action action/remove))

    (arg '((-rad --remove-and-delete))
         #:help "Remove and delete torrents"
         #:kons (options-add-action action/remove-and-delete))))

(define (process-args args)
  (define knil
    (make-options
      #:actions '()
      #:errors '()
      #:help #f
      #:torrent '())) ; Act on no torrent
  (let* ((ret (process-arguments *OPTS* knil args)))
    (*usage* (lambda (pn) (print "Usage: " pn " [OPTION ...]")))
    (*program-name* (program-name))
    (update-options
      ret #:actions (reverse (options-actions ret))
      #:errors (reverse (options-errors ret)))))

(define (main args)
  (let ((options (process-args args)))
    (cond
      ; Any errors processing the arguments?
      ((not (null? (options-errors options)))
       (when (options-help options) (help *OPTS*))
       (errors-show (options-errors options)))
      ((options-help options)
       (help *OPTS*))
      (else (actions-run (options-actions options))))))

(main (command-line-arguments))
