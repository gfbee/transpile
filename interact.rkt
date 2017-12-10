#lang racket

(provide show interact out)

(require (for-syntax syntax/parse))

(define out (curryr println (current-output-port) 1))

; Used at the top level, prints each expression e as (★ e), followed by the value of e.
#;(show e ...)
(define-syntax show (syntax-parser [(_ e ...) #'(begin (begin (out '(★ e)) e) ...)]))

#;(interact id
            expr
            ...)
; Interactively: repeatedly do (show expr ...), where the expressions [can] depend on id,
;  but asking the user each time for the value of id, until they enter the word ‘end’.
(require racket/pretty)
(define-syntax interact
  (syntax-parser [(_ an-id:id e:expr ...+)
                  #'(local []
                      (printf "★ Delayed ★\n")
                      (for-each out '(e ...))
                      (printf "★ Enter a value for ~a, or enter the word end ★\n" 'an-id)
                      (let loop ()
                        (flush-output)
                        (define an-id (read))
                        (unless (equal? an-id 'end)
                          (pretty-print e)
                          ...
                          (loop))))]))

; ToDo. File error message issue for:
#;(require a ())
