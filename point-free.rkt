#lang racket

(provide (rename-out [=? =] [>? >] [<? <] [<=? <=] [>=? >=]
                     [equal?? equal?] [equal?? ≡]
                     [<=? ≤] [>=? ≥]
                     [compose ∘]
                     [not/c ¬] [and/c ∧] [or/c ∨]))

#| Overloaded numeric comparisons, producing unary predicates.

 Curried for unary case: (<rel> c) ≡ (curry <rel> c).

 For a function f: (<rel> f c) is a unary predicate that checks if f of a value has the relation to c.
   That's meant for use with HOFs, but here's the algebraic rule: ((<rel> f c) x) ≡ (<rel> (f x) c).
   The rule demonstrates that immediate use is akward with potentially confusing order.

 Otherwise (<rel> x ...) ≡ (<rel> x ...).

 Potential generalizations, keeping in mind that overloading increases difficulty of debugging:
   • ((=? f c) x ...) ≡ (= (f x ...) c) - probably useful
   • for ‘equal?’ - probably useful
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
  (check-equal? (filter (=? length 2)            '((1) (1 2) (1 2 3)))
                (filter (λ (l) (= (length l) 2)) '((1) (1 2) (1 2 3))))

  ; Curried.
  (check-equal? (filter (<? 1) '(3 1 4 1 5 9)) '(3 4 5 9)))

(define (overload p)
  (case-lambda [(f/v a) (if (procedure? f/v) (λ (b) (p (f/v b) a)) (p f/v a))]
               [(v) (λ (v′) (p v v′))]
               [args (apply p args)]))

(define =?  (overload =))
(define <?  (overload <))
(define <=? (overload <=))
(define >?  (overload >))
(define >=? (overload >=))
(define equal?? (overload equal?))

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

; To come:
;   unequal? ≠
;   nor/c
;   ∘
