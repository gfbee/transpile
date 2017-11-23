#lang racket

(provide depth prune share)

(module+ test (require rackunit)
  
  ; Quick tests/examples of nested lists, by just quoting some code.
  (define tree-0 '(define (depth t)
                    (if (list? t)
                        (add1 (apply max 0 (map depth t)))
                        0)))

  (define tree-1 '(define (prune n t)
                    (if (list? t)
                        (if (zero? n)
                            '(⋯)
                            (map (curry prune (sub1 n)) t))
                        t)))
  
  (check-equal? (depth tree-0) 5)
  
  (check-equal? (map (curryr prune tree-0) (range (add1 (depth tree-0))))
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
  
  (check-equal? (share (list tree-1 tree-0))
                '(define ★
                   (if (list? t)
                       ★
                       ★))))

; n levels of tree
(define (prune n tree)
  (if (list? tree)
      (if (zero? n)
          '(⋯)
          (map (curry prune (sub1 n)) tree))
      tree))

(define (depth t)
  (if (list? t)
      (add1 (apply max 0 (map depth t)))
      0))

; shared top of list of trees
(define (share trees [difference-marker '★])
  (define (share′ t1 t2)
    (cond [(equal? t1 t2) t1]
          [(and (list? t1) (list? t2) (= (length t1) (length t2)))
           (map share′ t1 t2)]
          [else difference-marker]))
  (for/fold ([t (first trees)])
            ([t′ (rest trees)])
    (share′ t t′)))
