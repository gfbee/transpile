#lang racket #| Lists as trees |#

#| Two common ways to view a list as a tree:

 1. Lists are internal nodes with no data, containing their children,
     with data/labels only at the non-list leaves.
 2. Lists are nodes with data and children, with the data as the first element
     and the children as the rest of the element.

 Some wrinkles:
   • empty lists (1, 2)
   • non-list children (2)
   • list data (2)

 Leaves (1) with data (2): racket's ‘flatten’ [non-list elements].
 Data (2): ‘tags’ [first elements].
 Sub-trees (1) with data (2): ‘parts’ [elements].

 Depth: ‘depth’.

 Top n levels: ‘prune’.
 Top shared between trees: ‘share’. |#

(provide size height width widths
         tags parts
         prune share
         index)

; Records intent.
; Any value does work wherever this is used: non-lists are simply treated atomically.
(require racket/contract)
(define tree/c any/c)

#| Measurement |#

(define size/c            (tree/c . -> . natural?))
(define height/c          (tree/c . -> . natural?))
(define width/c  (natural? tree/c . -> . natural?))
(define widths/c (tree/c . -> . (listof natural?)))

(module* contracted #false
  (provide (contract-out [size size/c]
                         [height height/c]
                         [width width/c]
                         [widths widths/c])))

(require (only-in "point-free.rkt" ∘))
(define size (∘ length flatten))
(define (height t) (if (list? t) (add1 (apply max 0 (map height t))) 0))
(define (width n t) (- (size (prune (add1 n) t))
                       (size (prune       n  t))))
(define (widths t) (map (curryr width t) (range (height t))))


#| --- |#

(require racket/trace) ; See usage of (trace <id>) later in the code.

(module+ test (require rackunit)
  
  ; Quick tests/examples of nested lists, by just quoting some code.
  (define-values (tree-0 tree-1) (values '(define (depth t)
                                            (if (list? t)
                                                (add1 (apply max 0 (map depth t)))
                                                0))
                                         '(define (prune n t)
                                            (if (list? t)
                                                (if (zero? n)
                                                    '(⋯)
                                                    (map (curry prune (sub1 n)) t))
                                                t))))
  (check-equal? (height tree-0) 5)
  (check-equal? (map (curryr prune tree-0) (range (add1 (height tree-0))))
                '((⋯)
                  (define (⋯)
                    (⋯))
                  (define (depth t)
                    (if (⋯)
                        (⋯)
                        0))
                  (define (depth t)
                    (if (list? t)
                        (add1 (⋯))
                        0))
                  (define (depth t)
                    (if (list? t)
                        (add1 (apply max 0 (⋯)))
                        0))
                  (define (depth t)
                    (if (list? t)
                        (add1 (apply max 0 (map depth t)))
                        0))))
  
  (check-equal? (share (list tree-1 tree-0)) '(define ★
                                                (if (list? t)
                                                    ★
                                                    ★)))
  
  (check-equal? (tags tree-0) '(define depth if list? add1 apply map))
  (check-equal? (parts tree-0)
                '((define (depth t) (if (list? t) (add1 (apply max 0 (map depth t))) 0))
                  define
                  (depth t)
                  depth
                  t
                  (if (list? t) (add1 (apply max 0 (map depth t))) 0)
                  if
                  (list? t)
                  list?
                  t
                  (add1 (apply max 0 (map depth t)))
                  add1
                  (apply max 0 (map depth t))
                  apply
                  max
                  0
                  (map depth t)
                  map
                  depth
                  t
                  0)))

(define (prune n tree)
  (if (list? tree)
      (if (zero? n)
          '(⋯)
          (map (curry prune (sub1 n)) tree))
      tree))

#;(check-equal? (tags ast) (map first (filter list? asts)))
#;(check-equal? (tags ast) (map first (filter list? ((sxpath '(//)) ast))))

(define (tags t)
  (match t
    [`(,label . ,t) `(,label . ,(append-map tags t))]
    [else '()]))

(define (parts t)
  (list* t (if (list? t)
               (append-map parts t)
               '())))

(define (share trees [difference-marker '★])
  (define (share′ t1 t2)
    (cond [(equal? t1 t2) t1]
          [(and (list? t1) (list? t2) (= (length t1) (length t2)))
           (map share′ t1 t2)]
          [else difference-marker]))
  #;(trace share′)
  (for/fold ([t (first trees)])
            ([t′ (rest trees)])
    (share′ t t′)))

#;(trace depth prune share)

(define (index t an-index)
  (dict-ref an-index t
            (λ () (if (list? t)
                      (map (curryr index an-index) t)
                      t))))

