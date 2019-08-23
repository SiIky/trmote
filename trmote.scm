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

;; @brief The options processing state
;; @param actions A list of actions to run a certain target
;; @param errors A list of arguments parsing errors
;; @param help? Should show help?
;; @param target The torrent(s) the actions will act on. Can be #f for no
;;        targets, torrents for a list of IDs or hashes, or filename
;;        for a filename or magnet
;; @param target-actions The actions for the current target
;;
;; TODO: How to promote @a target-actions to @a actions
;; Before @a target is changed, with `-l` or `-a`, @a target-actions is
;; promoted to @a actions.
(defstruct ost actions errors help? target target-actions)

(define (filename str) `(filename . ,str))
(define (filename? x) (eq? (car x) 'filename))
(define (torrents tors) `(torrents . ,tors))
(define (torrents? x) (eq? (car x) 'torrents))
(define target cdr)
(define false? not)

; TODO: Replace with a comparse implementation
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
        ((string=? str "all") `(#t . ,(torrents #f)))
        ((string=? str "active") `(#t . ,(torrents "recently-active")))
        ((id-int str) => (lambda (res) `(#t . ,(torrents res))))
        (else `(#f . #f)))
      '(#f . #f)))

(define (!f? x f) (and x (f x)))
(define (call proc) (proc))

;;
;; Error procedures
;;

(define (error-show error) (call error))
(define (errors-show errors) (for-each error-show errors) (exit (length errors)))
(define (ost+errmsg ost . rest)
  (update-ost ost #:errors `(,(cut apply print rest) . ,(ost-errors ost))))

;;
;; Actions procedures
;;

(define (show-results results show)
  (let ((results (vector->list results)))
    (let ((arguments (!f? (assoc "arguments" results) (compose vector->list cdr)))
          (result (!f? (assoc "result" results) cdr)))
      (if (and result (string=? result "success"))
          (show arguments)
          (print "The server replied with: " result)))))

(define (action-run action) (call action))
(define (actions-run actions) (for-each action-run actions))
(define ((ost+action make-act) ret switch args)
  (update-ost
    ret #:actions
    `(,(make-act ret switch args) . ,(ost-actions ret))))

(define ((action/list ost switch args))
  (let ((ids (ost-target ost))
        (fields '("id" "name")))
    (when ids ; Temporary workaround. Actions with no target are an error.
      (let ((res (torrent-get fields #:ids (target ids))))
        (show-results res pp)))))

(define ((action/remove ost switch args))
  (print "Removing torrents: " (target (ost-target ost))))
(define ((action/remove-and-delete ost switch args))
  (print "Removing and deleting torrents: " (target (ost-target ost))))

(define (actions+target-actions actions target-actions)
  (cons (cut actions-run target-actions) actions))

(define (promote-actions ost)
  (cond
    ((and (false? (ost-target ost)) ; no previous target
          (null? (ost-target-actions ost))) ; no target actions
     ost)
    ((false? (ost-target ost)) ; target actions, but no target
     (ost+errmsg ost "Actions with no target: " (ost-target-actions ost)))
    (else
      (let ((actions (ost-actions ost))
            (target-actions (ost-target-actions ost)))
        (update-ost
          ost
          #:actions (actions+target-actions actions target-actions)
          #:target-actions '())))))

;; CLI Grammar
(define *OPTS*
  (cling
    (lambda (ret _ rest)
      (if (null? rest) ret
          (ost+errmsg ret "The following arguments will be ignored -- " rest)))

    ;; Server connection switches

    (arg '((-? -h --help))
         #:help "Show this help message"
         #:kons (lambda (ret _ _) (update-ost ret #:help? #t)))

    (arg '((--host) . host)
         #:help "Hostname of the Transmission daemon"
         #:kons (lambda (ret _ host) (*host* host) ret))

    (arg '((--port) . port)
         #:help "The RPC server port" #:kons
         (lambda (ret _ port)
           (let ((port-n (string->number port)))
             (if port-n
                 (begin (*port* port-n) ret)
                 (ost+errmsg ret "Not a valid port: " port)))))

    (arg '((--url) . url)
         #:help "The RPC server path"
         ; FIXME: Pass as /path/to/rpc and split by / maybe
         #:kons (lambda (ret _ url) (*url* url) ret))

    (arg '((--username) . username)
         #:help "The RPC username"
         #:kons (lambda (ret _ username) (*username* username) ret))

    (arg '((--password) . password)
         #:help "The RPC password"
         #:kons (lambda (ret _ password) (*password* password) ret))

    ;; Target switches

    ; TODO: Implement this
    ; One way to do this is to get the ID of the newly added torrent from the
    ; result of `torrent-add`, and use that as the target for future calls, but
    ; `download-dir` doesn't work right this way.
    (arg '((-a --add) . filename/magnet)
         #:help "Add torrents to transmission" #:kons
         (lambda (ret _ filename/magnet)
           (update-ost (promote-actions ret) #:target #f)))

    (arg '((-t --torrent) . torrents)
         #:help "The target torrents" #:kons
         (lambda (ret _ torrents)
           (let* ((res (string->torrents-list torrents))
                  (ret (update-ost (promote-actions ret) #:target (cdr res))))
             (if (car res) ret
                 (ost+errmsg ret "Not a valid torrents list: " torrents)))))

    ;; Action switches

    (arg '((-l --list))
         #:help "List torrents"
         #:kons (ost+action action/list))

    (arg '((-r --remove))
         #:help "Remove torrents"
         #:kons (ost+action action/remove))

    (arg '((-rad --remove-and-delete))
         #:help "Remove and delete torrents"
         #:kons (ost+action action/remove-and-delete))))

(define (process-args args)
  (define knil
    (make-ost
      #:actions '()
      #:errors '()
      #:help? #f
      #:target #f
      #:target-actions '()))
  (let* ((ret (process-arguments *OPTS* knil args)))
    (*usage* (lambda (pn) (print "Usage: " pn " [OPTION ...]")))
    (*program-name* (program-name))
    (update-ost
      ret #:actions (reverse (ost-actions ret))
      #:errors (reverse (ost-errors ret)))))

(define (main args)
  (let ((ost (process-args args)))
    (cond
      ; Any errors processing the arguments?
      ((not (null? (ost-errors ost)))
       (when (ost-help? ost) (help *OPTS*))
       (errors-show (ost-errors ost)))
      ((ost-help? ost)
       (help *OPTS*))
      (else (actions-run (ost-actions ost))))))

(main (command-line-arguments))
