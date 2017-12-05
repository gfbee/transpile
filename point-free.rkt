#lang racket

(provide (rename-out [=? =]))

#| Overloaded ‘=’.

 To make unary predicates that check if a particular function of a value equals a particular number.

 If f is a function, then (=? f c) makes a unary predicate that checks if f of a value is equal to c.
  It's meant to be used with HOFs, but here is the algebraic rule [which shows that immediate use
  is no shorter, and with a possibly confusing order of operations]:

    If f is a function, ((=? f c) x) ≡ (= (f x) c) ≡ (= c (f x)).
    Otherwise (=? e ...) ≡ (= e ...).

 Potential generalizations, keeping in mind that overloading increases difficulty of debugging:
   • ((=? f c) x ...) ≡ (= (f x ...) c) - probably useful
   • for ‘equal?’ - probably useful
   • curried (=? f) - could be useful
   • (=? f c c′ cs ...) - least useful |#

; Example.
(module+ test (require rackunit)
  ; Use overloaded ‘=’ to make a unary predicate checking if a list has length two.
  (check-true  ((=? length 2) '(anya buffy)))
  (check-false ((=? length 2) '(anya buffy willow)))
  ;
  (check-true (procedure? (=? length 2)))
  (check-equal? (procedure-arity (=? length 2)) 1)
  ;
  (check-equal? (filter (=? length 2) '((1) (1 2) (1 2 3))) '((1 2)))
  ;
  (check-equal? (filter (=? length 2)
                        '((1) (1 2) (1 2 3)))
                (filter (λ (l) (= (length l) 2))
                        '((1) (1 2) (1 2 3)))))

(define =? (case-lambda [(f/v a) (if (procedure? f/v) (λ (b) (= (f/v b) a)) (= f/v a))]
                        [args (apply = args)]))


; Reference for doing such things based on static form.
#;(require (for-syntax syntax/parse)
           (prefix-in racket: (only-in racket/base =)))
#;(define-syntax =
    (syntax-parser
      [f:id #'racket:=]
      [(_ f:expr a:expr) #'(let ([f′ f] [a′ a])
                             (if (procedure? f)
                                 (λ (b) (racket:= (f b) a′))
                                 (racket:= f′ a′)))]
      [(_ . sub-forms) #'(racket:= . sub-forms)]))
