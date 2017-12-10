#lang racket/base

(provide read-ast)

; ghc-dump-tree --json MaybeInt.hs > MaybeInt.json
(require json (only-in racket/dict in-dict) racket/list racket/function)

(define (read-ast filename)
  (define p (open-input-file filename #;"Stack.json" #;"MaybeInt.json"))
  (void (read-line p)) #;"in treesForTargets"
  (collapse (remove "PlaceHolder" (remove 'location (read-json p)))))

(define (remove key j)
  (cond [(hash? j) (list (for/hash ([(k v) (in-dict j)] #:unless (equal? k key))
                           (values k (remove key v))))]
        [(list? j) (append-map (curry remove key) j)]
        [(equal? j key) (list)]
        [else (list j)]))

(define (collapse j)
  (define (L result) (if (= (length result) 1) (first result) `(: . ,result)))
  (cond [(hash? j) (L (for/list ([(k v) (in-dict j)]) (list k (collapse v))))]
        [(list? j) (L (map collapse j))]
        [else j]))
