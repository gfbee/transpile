#lang typed/racket

(provide chop)

(module+ test #;(require rackunit)
  (define check-equal? equal?)
  #;(define/contract chop′ chop/c chop)
  (define chop′ chop)
  (: chop′′ Chop)
  (define (chop′′ p a-list)
    (define r  (chop′    p  a-list))
    (define r′ (chop′ (¬ p) a-list))
    (check-equal? r′ r)
    r)
  ; Without parametricity, typechecking use of even? produces:
  ;   expected: (-> Any Any)
  ;   given: (-> Integer Boolean)
  (check-equal? (chop′′ even? '()) '()) ; 
  (check-equal? (chop′′ even? '(1)) '((1)))
  (check-equal? (chop′′ even? '(1 3 5)) '((1 3 5)))
  (check-equal? (chop′′ even? '(1 3 5 2)) '((1 3 5) (2)))
  (check-equal? (chop′′ even? '(1 3 5 2 4 6)) '((1 3 5) (2 4 6)))
  (check-equal? (chop′′ even? '(1 3 5 2 4 6 1)) '((1 3 5) (2 4 6) (1))))

(define-type Chop (∀ (α) (α → Any) (Listof α) →
                     ; Any of these work, for untyped uses of the result.
                     #;Any
                     #;(Listof Any)
                     (Listof (Listof α))))

(: chop : Chop)
(define (chop p a-list)
  (cond [(empty? a-list) '()]
        [else (define-values (initial-ps the-rest) (splitf-at a-list p))
              (define chopped-rest (chop (¬ p) the-rest))
              (if (empty? initial-ps)
                  chopped-rest
                  (list* initial-ps chopped-rest))]))

#;(require/typed #;racket/contract
                 #;(only-in "point-free.rkt" ¬)
                 ; Generated contract complains with subtle error at runtime.
                 "point-free.rkt" (¬ (∀ (α) ((α → Any) → (α → Boolean)))))

; Works.
#;{(: ¬ : (∀ (α) ((α → Any) → (α → Boolean))))
   (define ((¬ p) v) (not (p v)))}
; Doesn't work.
#;(define ¬ (curry compose not))

(define ¬ negate)

#;(define chop/c ((any/c . -> . any/c) list? . -> . (listof list?)))




