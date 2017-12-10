#lang racket
(require rackunit)
; Types
; =====
; Integer
; List T
; Tuple T ...
; T → T
; C T ...
;
; type
;   synonym
; data
;   new ADT

; Identifier declaration
; ----------------------
; id :: T
;   usually id :: T → T


; (<expr> <expr>)
;
; let <pat> = <expr>
;     ...
;     in <expr>
;
; <literal>
;

; ghc-dump-tree --json MaybeInt.hs > MaybeInt.json
(module JST racket/base (provide ast)
  (require json (only-in racket/dict in-dict) racket/list racket/function)
  (define p (open-input-file "Stack.json" #;"MaybeInt.json"))
  (void (read-line p)) #;"in treesForTargets"
  (define j (read-json p))
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
  (define ast (collapse (remove "PlaceHolder" (remove 'location j)))))

(require 'JST)

#| Very General Helpers |#

; Usages below, and their behaviour in the Interactions, are shortest explanation.
; Otherwise: see the comments in "interact.rkt".
(require (only-in "interact.rkt" show interact))

; Group (list e ...) by (key1 e), group within those by (key2 e), etc.
#;(group key1 key2 ... a-list)
(require (only-in "equivalence.rkt" group))

(require (only-in "point-free.rkt" = > < ≥ >= ≤ <= ≡ equal?))
; Overloaded, and symbolic aliases.
; Unary use produces unary version of the binary version, with first argument fixed.
; Binary use, with first argument a function f, produces unary version of the bianary version,
;  that calls f and compares with the second argument
#;((= height 1) ast) ; Does height of ast = 1?
#;(show (remove-duplicates (filter (= height 2) asts))) ; All height 2 parts of the ast.

(require (only-in "point-free.rkt" ∘)) ; Common name for ‘compose’.

#| General Tree Helpers |#
(require (except-in "tree-query.rkt" size height width widths)
         (only-in (submod "tree-query.rkt" contracted) size height width widths)
         sxml)

#| Tree Measurements |#
(show (size ast) (height ast)
      (define ws (widths ast))
      ws
      (sort #:key car (counts ws) <)
      (apply max ws))

#| Tree Shape |#

; draw-widths : (any/c . -> . image?)
(define (draw-widths t)
  (local-require (only-in 2htdp/image rectangle))
  (apply above (map (curryr rectangle 1 "solid" "black") (widths t))))
(require (only-in 2htdp/image scale scale/xy))

; EXAMPLE:
(show (define dws (draw-widths ast)) dws (scale/xy 1 3 dws) (scale 3 dws))

(define (image t)
  (define • (circle 1 "solid" "black"))
  (define ∘ (square 2 "solid" "transparent"))
  (define i (square 0 "solid" "transparent"))
  (cond [(list? t)
         (define kids (apply beside/align "top" i i (add-between (map image (rest t)) ∘)))
         (above •
                (rectangle (max 2 (- (image-width kids) 4)) 1/2 "solid" "black")
                kids)]
        [else •]))

(interact d (scale 3 (image (prune d ast))))

; structure : (any/c . -> . any/c)
; This is where the big area of pretty-printing starts to come in.
; Since it's about structure at this point, shrinkable output in the Interactions
;  would be a good first step.
(define (structure t) (if (list? t) (map structure t) '•))

; EXAMPLE:
(interact d (prune d ast) (prune d (structure ast)))

; Pretty-printing.
(show (parameterize ([pretty-print-columns (* 3 (height ast)) #;'infinity])
        #;(pretty-print (structure ast))
        ; pretty-print-depth is similar to my ‘prune’
        (parameterize ([pretty-print-depth (quotient (height ast) 4)])
          (pretty-print (structure ast))
          (pretty-print ast))))
;
; ToDo: newline for arity > 2
;       arity indicators

; ToDo: document filter-map, consider multi-list filters.

; Explore the arities.
(require (only-in "equivalence.rkt" counts)
         (only-in "point-free.rkt" ¬ ∧ ∨))
(show (define asts (parts ast))
      (define arity (∧ list? (∘ sub1 length)))
      (define arities (sort (filter-map arity asts) <))
      #;(for/list ([a 4]) (count (= a) arities))
      (filter (≡ arity 0) asts)
      (prune 4 (filter (≡ arity (apply max arities)) asts))
      #;(filter (< 2) arities)
      #;(samples->hash arities) ; doesn't preserve ordering
      (counts arities)
      (prune 4 (filter (≡ arity 5) asts)))
(interact d (prune d (group second third (filter (∧ list? (> length 2)) asts))))

#| Bottom-up |#

; Interpreted unlabelled.
;   height = 0
(show (define atoms (flatten ast))
      (check-equal? atoms (filter (= height 0) asts))
      ; It's only symbols and strings.
      (sort (remove-duplicates (filter symbol? atoms)) symbol<?)
      (sort (remove-duplicates (filter string? atoms)) string<?)
      (check-pred zero? (count (¬ (∨ symbol? string?)) atoms))
      #;(check-pred empty? (filter-not (∨ symbol? string?) leaves)))
(interact c (filter (≥ cdr c) (counts atoms)))
;  height ≥ 1
(interact d (group-by first (filter (= height d) asts)))

#| Tags/Labels |#
(show (check-true (andmap symbol? (tags ast)))) ; All are symbols.
(interact c (filter (≥ cdr c) (counts (tags ast))))

#| --- |#

(require (only-in "order.rkt" chop))

(define (s t)
  (if (list? t)
      (append-map (λ (segment) (if (symbol? (first segment))
                                   (list (string->symbol (apply ~a segment)))
                                   segment))
                  (chop symbol? (map s t)))
      #;'• t))

(require (only-in 2htdp/image square circle beside image-width beside/align above/align
                  rectangle above scale))
(define (draw t)
  (define • (circle 1 "solid" "black"))
  (define ∘ (square 2 "solid" "transparent"))
  (cond [(list? t) (define kids (apply beside/align "top"
                                       ∘ (append (map draw t) (list ∘))))
                   (beside ∘ (above/align "left"
                                          (rectangle (- (image-width kids) 2) 1 "solid" "black")
                                          kids) ∘)]
        [else •]))
(show #;(structure ast)
      #;(s ast)
      #;(scale 1/2 (draw ast)))

#| --- |#

; No symbols as first kid.
(show #;(sift symbol? second (filter (∧ list? (≥ length 2)) asts))
      (count (∧ list? (≥ length 2) (∘ symbol? second)) asts))

; Flatten unary chains of tags into initial segment of tags.
(define (inline t) (if (list? t)
                       (match t
                         [`(,tag ,(? list? t′)) `(,tag . ,(inline t′))]
                         [_ (map inline t)])
                       t))

(show (define a (inline ast)) (height (s a)))
(interact d (prune d a) (prune d (s a)))

(show #;(length (remove-duplicates (filter symbol? (flatten (s a)))))
      (count symbol? (remove-duplicates (flatten (s a)))))

(interact c (filter (≥ cdr c) (sort (counts (filter symbol? (flatten (s a)))) #:key cdr <)))

#;(show (define shares (filter (λ (st) (member '★ (flatten st)))
                               (map share
                                    (group first (filter (∧ list? (≥ length 2)) (parts (s a)))))))
        (group first (filter list? shares))
        (filter (> length 1) (group first (filter list? shares))))

#;{(height a) (structure a) (length (parts a))
              (group first (filter list? (parts a)))
              (prune 2 (group first (filter list? (parts a))))
              (prune 3 (group first (filter list? (parts a))))
              (prune 3 (group first second (filter list? (parts a))))
              (prune 4 (group first second (filter list? (parts a))))
              (prune 4 (group first second third (filter list? (parts a))))
              (prune 4 (group first second third (filter (∧ list? (≥ length 3)) (parts a))))
              (prune 5 (group first second third (filter (∧ list? (≥ length 3)) (parts a))))
              (filter (∧ list? (≥ length 2) (∘ symbol? second)) (parts a))
              (map (curryr takef symbol?) (filter (∧ list? (≥ length 2) (∘ symbol? second))
                                                  (parts a)))
              (group first second (map (curryr takef symbol?)
                                       (filter (∧ list? (≥ length 2) (∘ symbol? second))
                                               (parts a))))
              (group identity (map (curryr takef symbol?)
                                   (filter (∧ list? (≥ length 2) (∘ symbol? second))
                                           (parts a))))}

#| Repeated Parts |#
(require (only-in math/statistics samples->hash))
(define ast′ (s (inline ast)))
(define part×reps (hash-map (samples->hash (filter list? (parts ast′))) list))
(define-values (part reps) (values first second))

#;(define (sift p selector relation) (filter (∘ p selector) relation))
(define repeats (map part (filter (≥ reps 2) part×reps)))
(define an-index (map cons repeats (range (length repeats))))
(define indexed-ast′ (index ast′ an-index))

(show (size ast) (size (inline ast)) (size (s (inline ast))) (size indexed-ast′)
      (size an-index) (+ (size an-index) 261))
(show (image ast)
      (image (inline ast))
      (image (s (inline ast)))
      (image indexed-ast′))
(interact d (prune d ast′) (prune d indexed-ast′))
; Repeats can become singles when all but one use is inside another repetition.
(show (define singles (filter (= cdr 1) (counts (filter number? (flatten indexed-ast′)))))
      (map (curry dict-ref (map (match-lambda [`(,a . ,b) `(,b . ,a)]) an-index))
           (map car singles)))
(interact d (filter (≥ cdr 2) (counts (prune d (filter list? (parts indexed-ast′))))))


#| SXPath |#
(show (prune 2 ast)
      (take (prune 2 asts) 10)
      (take (prune 2 ((sxpath '(//)) ast)) 10)
      (andmap string? (filter (¬ list?) ((sxpath '(//)) ast)))
      ((sxpath '(// VarName)) ast)
      (prune 4 ((sxpath '(// TyClD)) ast))
      (prune 3 ((sxpath '(// TyClD SynDecl)) ast))
      (≡ ((sxpath '(// TyClD SynDecl)) ast)
         ((sxpath '(// TyClD / SynDecl)) ast))
      ((sxpath '(// SigD // VarName)) ast)
      ((sxpath '(// (*or* TvName TcClsName))) ast))

(show (group first second (prune 3 ((sxpath "//VarName/..") ast)))
      (group first second (filter (= height 3) asts)))


#| --- |#

; Labelled nodes.
; Maximal subtrees where root node label is key/s or in key/s.
(define (find key/s t)
  (match t
    ['() '()]
    [`(,h . ,t) (if (or (and (list? key/s) (member h key/s))
                        (≡ h key/s))
                    (list t)
                    (append-map (curry find key/s) t))]
    [else (list)]))


#;{(define DataDecl (find 'DataDecl ast))
   (define data (first (find 'TcClsName DataDecl)))
   (define constructors
     (map (λ (e) (append (find 'DataName e) (find 'TcClsName e)))
          (find 'ConDeclH98 DataDecl)))
   (define deriving (find 'TcClsName (find 'HsDerivingClause DataDecl)))

   (define clauses (find 'Match ast))
   (define fs (find 'VarName (find 'FunRhs clauses)))
   (define patterns (list (find 'VarName (find 'VarPat clauses))
                          (map second (find 'HsIntegral (find 'NPat clauses)))))
   (define results (find 'GRHS ast))}

#;(map list fs patterns results)

#;(define (unparse . ast) (apply ~a (add-between (flatten ast) " ")))
#;(unparse f ':: (add-between f-type '->))
#;(unparse 'data data '= (add-between constructors '\|) 'deriving deriving)

#;(define symbol string->symbol)
#;(define (from-singleton l) (match l [`(,e) e]))
#;(define (untag pair) (match pair [`(,_ ,e) e]))
#;(define find1 (∘ from-singleton find))

#;{(define module (filter (¬ (curry ≡ "Nothing"))
                          (untag (find1 'HsModule ast))))
   (define top-kinds '(TyClD SigD ValD))
   (define top (find top-kinds ast))
   (check-≡ (map first module) (map first top))

   (define top-kinds′ (group-by identity (prune 4 top)))
   (check-≡ (length top-kinds) (length top-kinds′))
   (check-≡ (map first top-kinds′)
            '((TyClD (SynDecl #;DataDecl ((⋯) (⋯) "Prefix" (⋯))))
              (SigD (TypeSig ((⋯) (⋯))))
              (ValD (FunBind ((⋯) (⋯) "WpHole")))))

   (define signatures (find 'SigD ast))
   (share signatures)
   (share (find 'ValD ast))
   (share (find 'TyClD ast))

   (define (SigD t)
     (match t
       [`(SigD . ,t) (list (symbol (untag (find1 'VarName t)))
                           (FunTy (find1 '(HsFunTy HsAppsTy) t)))]))

   (define (thin key/s t)
     (define t′ (find key/s t))
     (if (empty? t′)
         t
         (map (match-lambda [`(,k . ,t) `(,k . ,(thin key/s t))])
              t′)))

   (thin '(TvName TcClsName HsTupleTy TyClD HsFunTy HsAppsTy) ast)

   (define (FunTy t)
     (match t
       [`(HsFunTy (,a ,b)) `(,(FunTy a) → ,(FunTy b))]
       [`(HsAppsTy . ,t) (match (find 'HsFunTy t)
                           [`(,ft) (FunTy ft)]
                           [_ (match (find '(TvName TcClsName HsTupleTy) t)
                                [`((HsTupleTy ,t)) `(tuple ,(FunTy t))]
                                [`((,_ ,(app symbol id))) id]
                                [`((TcClsName ,(app symbol id-c)) (,_ #;TvName ,(app symbol id-a)))
                                 `(,id-c ,id-a)])])]))

   (map SigD)}