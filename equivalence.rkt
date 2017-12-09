#lang racket

#| Lists as bags, sets, equivalence classes. |#

(provide

 ; counts : (sequence? . -> . (listof (cons/c any/c natural?)))
 ; See also samples->hash, but counts is ordered by first occurrence of element.
 counts
 
 ; same API as ‘check-duplicates’ and ‘remove-duplicates’
 group-duplicates
 
 ; primary, secondary, ... keys
 ; group : (any/c . -> . any/c) ... list? -> list?
 group)

(module+ test (require rackunit)
  (check-equal? (group                 '(-1 2 3 -4 5)) '(-1 2 3 -4 5))
  (check-equal? (group negative?       '(-1 2 3 -4 5)) '((-1 -4)
                                                         (2 3 5)))
  (check-equal? (group negative? even? '(-1 2 3 -4 5)) '(((-1)
                                                          (-4))
                                                         ((2)
                                                          (3 5)))))

(define (group-duplicates a-list [same? equal?] #:key [extract-key (λ (v) v)])
  (group-by extract-key a-list same?))

; Since filtering doesn't change order and is a subset of the elements,
;  it's natural to use it to count the number of elements with a property.
;
; (count proc lst ...+) → exact-nonnegative-integer?
;   proc : procedure?
;   lst : list?
; For one list, (count p l) ≡ (length (filter p l)).
; For more than one list, it uses filter-map.
;
; ToDo: counts taking a predicate, unified with grouping, etc.

(define (counts seq)
  (local-require (only-in math/statistics count-samples #;samples->hash))
  (define-values (es cs) (count-samples seq))
  (map cons es cs))

#| Is it set-like?    check-duplicates: first duplicate, or #false.
   To set-like list. remove-duplicates: keep only first occurrences.

     ( check-duplicates a-list [same? #:key extract-key]) → (or/c any/c #false)
     (remove-duplicates a-list [same? #:key extract-key]) → list?
            a-list : list?
             same? : (any/c any/c . -> . any/c) = equal?
       extract-key : (any/c       . -> . any/c) = identity

   To list of equivalence classes. group-by: group each occurrence with any later ones.

     (group-by key a-list [same?]) → (listof list?)
               key : (any/c       . -> . any/c)
            a-list : list?
             same? : (any/c any/c . -> . any/c) = equal?

   See also: sets, statistics.

   ToDo
     • unnested ‘group’? labelled group? labels as database index? label-and-count only?
     • unify ‘group’ API with others?
     • why optional vs keyword in racket's API --- legacy?
     • generalize to sequences? |#

(define (group . keys-and-list)
  (match keys-and-list
    [`(,a-list) a-list]
    [`(,key ,keys ... ,a-list) (for/list ([g (group-duplicates a-list #:key key)])
                                 (apply group (append keys (list g))))]))
