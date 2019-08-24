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
  (only comparse
        ->parser-input
        any-of
        as-string
        bind
        fail
        in
        is
        item
        one-or-more
        parse
        preceded-by
        repeated
        result
        sequence
        zero-or-more)
  (only defstruct defstruct)
  (only srfi-1
        append-map
        concatenate
        every
        iota)
  (only srfi-14
        char-set:digit
        char-set:hex-digit)
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
;; targets, tgt-torrents for a list of IDs or hashes, or tgt-filename for a
;; filename or magnet
;; @param target-actions The actions for the current target
;;
;; TODO: How to promote @a target-actions to @a actions
;; Before @a target is changed, with `-l` or `-a`, @a target-actions is
;; promoted to @a actions.
(defstruct ost actions errors help? target target-actions)

(define (tgt-filename str) `(tgt-filename . ,str))
(define (tgt-filename? x) (eq? (car x) 'tgt-filename))
(define (tgt-torrents tors) `(tgt-torrents . ,tors))
(define (tgt-torrents? x) (eq? (car x) 'tgt-torrents))
(define tgt-unwrap cdr)
(define false? not)
(define (!f? x f) (and x (f x)))
(define (call proc) (proc))

(define (string->torrents-list str)
  (define (enum-from-to from to) (iota (add1 (- to from)) from))
  (define (times x parser) (repeated parser #:min x #:max x))
  (define (as-number parser) (bind (as-string parser) (o result string->number)))
  (define digit (in char-set:digit))
  (define hexdigit (in char-set:hex-digit))
  (define number (as-number (one-or-more digit)))
  (define (list-of parser) (bind parser (o result list)))
  (define torrent-hash (list-of (as-string (times 40 hexdigit))))
  (define torrent-id (list-of number))

  (define torrent-interval
    (bind (sequence torrent-id (is #\-) torrent-id)
          (lambda (id-id)
            (let ((from (caar id-id))
                  (to (caaddr id-id)))
              (lambda (rest)
                (if (<= from to)
                    (cons (enum-from-to from to) rest)
                    (fail rest)))))))

  (define torrent (any-of torrent-interval torrent-hash torrent-id))
  (define comma-torrent (preceded-by (is #\,) torrent))
  (define torrent-list
    (bind (sequence torrent (zero-or-more comma-torrent))
          (lambda (tor-tor)
            (result (concatenate (cons (car tor-tor) (cadr tor-tor)))))))

  (cond
    ((string=? str "all") `(#t . ,(tgt-torrents #f)))
    ((string=? str "active") `(#t . ,(tgt-torrents "recently-active")))
    ((parse torrent-list (->parser-input str))
     => (lambda (res) `(#t . ,(tgt-torrents res))))
    (else `(#f . #f))))

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
(define ((ost+target-action make-act) ret switch args)
  (let ((ta (make-act ret switch args))
        (tas (ost-target-actions ret)))
    (update-ost ret #:target-actions (cons ta tas))))

(define (((action/list _ _ _) ids))
  (assert (tgt-torrents? ids))
  (let ((fields '("id" "name"))
        (ids (tgt-unwrap ids)))
    (let ((res (torrent-get fields #:ids ids)))
      (show-results res pp))))

(define (((action/remove _ _ _) ids))
  (print "Removing torrents: " ids))
(define (((action/remove-and-delete _ _ _) ids))
  (print "Removing and deleting torrents: " ids))

(define ((prepare-target-actions target-actions target))
  (let* ((target (tgt-unwrap target))
         (actions (reverse (map (cut <> target) target-actions))))
    (actions-run actions)))

(define (ost-promote-target-actions ost)
  (let* ((target (ost-target ost))
         (target-actions (ost-target-actions ost))
         (actions (ost-actions ost))
         (ret (cond
                ((null? target-actions) ost)
                ((false? target)
                 (ost+errmsg ost "Actions with no target: " target-actions))
                (else (let ((actions-h (prepare-target-actions target-actions target)))
                        (update-ost ost #:actions (cons actions-h actions)))))))
    (update-ost ret #:target-actions '())))

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
           (let ((ret (ost-promote-target-actions ret)))
             (update-ost ret #:target (tgt-filename filename/magnet)))))

    (arg '((-t --torrent) . torrents)
         #:help "The target torrents" #:kons
         (lambda (ret _ torrents)
           (let ((tors (string->torrents-list torrents))
                 (ret (ost-promote-target-actions ret)))
             (if (car tors)
                 (update-ost ret #:target (tgt-torrents (cdr tors)))
                 (ost+errmsg ret "Not a valid torrents list: " torrents)))))

    ;; Action switches

    (arg '((-l --list))
         #:help "List torrents"
         #:kons (ost+target-action action/list))

    (arg '((-r --remove))
         #:help "Remove torrents"
         #:kons (ost+target-action action/remove))

    (arg '((-rad --remove-and-delete))
         #:help "Remove and delete torrents"
         #:kons (ost+target-action action/remove-and-delete))))

(define (process-args args)
  (define knil
    (make-ost
      #:actions '()
      #:errors '()
      #:help? #f
      #:target #f
      #:target-actions '()))
  (let* ((ret (ost-promote-target-actions (process-arguments *OPTS* knil args))))
    (*usage* (lambda (pn) (print "Usage: " pn " [OPTION ...]")))
    (*program-name* (program-name))
    (update-ost
      ret
      #:actions (reverse (ost-actions ret))
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
