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

; Show an expression and its result.
; Used at the top level, this prints each expression e as '(★ e), followed by the value of e.
(define-syntax-rule (show e ...) (begin (begin '(★ e) e) ...))

#;(interact id
            expr
            ...)
;
; Interactively: repeatedly do (show expr ...), where the expressions [can] depend on id,
;  but asking the user each time for the value of id.
;
(require (for-syntax syntax/parse) racket/pretty)
(define-syntax interact
  (syntax-parser [(_ an-id:id e:expr ...+)
                  #'(local [(define out (curryr println (current-output-port) 1))]
                      (printf "★ Delayed ★\n")
                      (for-each out '(e ...))
                      (printf "★ Enter a value for ~a, or enter the word end ★\n" 'an-id)
                      (let loop ()
                        (flush-output)
                        (define an-id (read))
                        (unless (equal? an-id 'end)
                          (for-each pretty-print (list e ...))
                          (loop))))]))


(require (only-in "equivalence.rkt" group))
; Group (list e ...) by (key1 e), group within those by (key2 e), etc.
#;(group key1 key2 ... a-list)

(require (only-in "point-free.rkt" = > < >= <= equal?))
; Overloaded:
;   • unary use produces unary version of the binary version, with first argument fixed
;   • binary use, with first argument a function f, produces unary version of the binary version,
;      that calls f and compares with the second argument
#;((= depth 1) ast) ; Does depth of ast = 1?
#;(show (remove-duplicates (filter (= depth 2) (parts ast)))) ; All depth 2 parts of the ast.

(require (only-in "point-free.rkt" ∘)) ; Common name for ‘compose’.

#| Filtering |#
(define (sift p selector relation) (filter (∘ p selector) relation))


#| General Tree Helpers |#
(require "tree-query.rkt" sxml)


#| Tree Measurements
 size   : (any/c . -> . natural?)
 depth  : (any/c . -> . natural?)
 width  : (natural? any/c . -> . natural?)
 widths : (any/c . -> . (listof natural?)) |#

(define size (∘ length flatten))
(require (only-in "tree-query.rkt" depth))
(define (width n t) (- (size (prune (add1 n) ast))
                       (size (prune       n  ast))))
(define (widths t) (map (curryr width t) (range (depth t))))

; EXAMPLE:
#;(show (size ast) (depth ast)
        (define ws (widths ast)) ws (sort ws <) (apply max ws))


#| Tree Shape |#

; draw-widths : (any/c . -> . image?)
(define (draw-widths t)
  (local-require (only-in 2htdp/image rectangle))
  (apply above (map (curryr rectangle 1 "solid" "black") (widths t))))
(require (only-in 2htdp/image scale scale/xy))

; EXAMPLE:
#;(show (define dws (draw-widths ast)) dws (scale/xy 1 3 dws) (scale 3 dws))

; structure : (any/c . -> . any/c)
; This is where the big area of pretty-printing starts to come in.
; Since it's about structure at this point, shrinkable output in the Interactions
;  would be a good first step.
(define (structure t) (if (list? t) (map structure t) '•))

; EXAMPLE:
(interact d (prune d ast) (prune d (structure ast)))

; Pretty-printing.
(show (pretty-print-columns (* 3 (depth ast)) #;'infinity)
      #;(pretty-print (structure ast))
      ; pretty-print-depth is similar to my ‘prune’
      (pretty-print-depth (quotient (depth ast) 4))
      (pretty-print (structure ast))
      (pretty-print ast))
;
; ToDo: newline for arity > 2
;       arity indicators

; ToDo: document filter-map, consider multi-list filters.

; Explore the arities.
(require (only-in "equivalence.rkt" counts)
         (only-in "point-free.rkt" ∧ ¬ ∨))
(show (define asts (parts ast))
      (define arity (∧ list? (∘ sub1 length)))
      (define arities (sort (filter-map arity asts) <))
      #;(for/list ([a 4]) (count (= a) arities))
      (filter (equal? arity 0) asts)
      (prune 4 (filter (equal? arity (apply max arities)) asts))
      #;(filter (< 2) arities)
      #;(samples->hash arities) ; doesn't preserve ordering
      (counts arities)
      (prune 4 (filter (equal? arity 5) asts))
      #;(group length second (prune 3 (filter (∧ list? (> length 2)) asts))))


#| --- |#

(define (chop p a-list)
  (if (empty? a-list)
      '()
      (let ([ps (takef a-list p)])
        (if (empty? ps) (chop (not/c p) a-list) (list* ps (chop (not/c p) (dropf a-list p)))))))

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
(show (sift symbol? second (filter (and/c list? (>= length 2)) (parts ast))))

; Flatten unary chains of tags into initial segment of tags.
(define (inline t) (if (list? t)
                       (match t
                         [`(,tag ,(? list? t′)) `(,tag . ,(inline t′))]
                         [_ (map inline t)])
                       t))

(show (define a (inline ast))
      (s a)
      (depth (s a))
      (length (remove-duplicates (filter symbol? (flatten (s a)))))
      (define shares (filter (λ (st) (member '★ (flatten st)))
                             (map share
                                  (group first (filter (and/c list? (>= length 2)) (parts (s a)))))))
      (group first (filter list? shares))
      (filter (> length 1) (group first (filter list? shares))))

#;{(depth a) (structure a) (length (parts a)) #;a
             (group first (filter list? (parts a)))
             (prune 2 (group first (filter list? (parts a))))
             (prune 3 (group first (filter list? (parts a))))
             (prune 3 (group first second (filter list? (parts a))))
             (prune 4 (group first second (filter list? (parts a))))
             (prune 4 (group first second third (filter list? (parts a))))
             (prune 4 (group first second third (filter (and/c list? (>= length 3)) (parts a))))
             (prune 5 (group first second third (filter (and/c list? (>= length 3)) (parts a))))
             (filter (and/c list? (>= length 2) (∘ symbol? second)) (parts a))
             (map (curryr takef symbol?) (filter (and/c list? (>= length 2) (∘ symbol? second))
                                                 (parts a)))
             (group first second (map (curryr takef symbol?)
                                      (filter (and/c list? (>= length 2) (∘ symbol? second))
                                              (parts a))))
             (group identity (map (curryr takef symbol?)
                                  (filter (and/c list? (>= length 2) (∘ symbol? second))
                                          (parts a))))}


#| Bottom-up |#

; Leaf data
(define leaves (flatten ast))


; It's only symbols and strings.
(sort (remove-duplicates (filter symbol? leaves)) symbol<?)
(sort (remove-duplicates (filter string? leaves)) string<?)
(check-pred empty? (filter (not/c (or/c symbol? string?)) leaves))
#;(check-pred empty? (filter-not (or/c symbol? string?) leaves))

(show (filter (= depth 0) (parts ast))
      (group-by first (filter (= depth 1) (parts ast)))
      #;(group-by first (filter (λ (p) (= (depth p) 1)) (parts ast))))


#| Tags/Labels |#

#;(show (empty? (filter list? (tags ast))) ; No compound tags.
        (andmap symbol? (tags ast)) ; In fact, all are symbols.
        (length (tags ast))
        ; Three ways to get tags.
        (equal? (tags ast) (map first (filter list? (parts ast))))
        (equal? (tags ast) (map first (filter list? ((sxpath '(//)) ast)))))

; Non-symbol/list tags [all would be strings, by above].
(filter (not/c (or/c symbol? list?)) (tags ast))


#| --- |#

; Labelled nodes.
; Maximal subtrees where root node label is key/s or in key/s.
(define (find key/s t)
  (match t
    ['() '()]
    [`(,h . ,t) (if (or (and (list? key/s) (member h key/s))
                        (equal? h key/s))
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

(define (unparse . ast) (apply ~a (add-between (flatten ast) " ")))
#;(unparse f ':: (add-between f-type '->))
#;(unparse 'data data '= (add-between constructors '\|) 'deriving deriving)

; Repeated sub-parts.
(require (only-in math/statistics samples->hash))
(define part×reps (hash-map (samples->hash (filter list? (parts (inline ast)))) list))
(define-values (part reps) (values first second))

(define repeats (map part (sift (<= 2) reps part×reps)))
(define an-index (map cons repeats (range (length repeats))))

(define symbol string->symbol)
(define (from-singleton l) (match l [`(,e) e]))
(define (untag pair) (match pair [`(,_ ,e) e]))
(define find1 (∘ from-singleton find))

#;(prune 7 ast)
#;(prune 8 ast)
(show (prune 8 (index (inline ast) an-index)))


#| SXPath |#
(show (prune 2 ast)
      (take (prune 2 (parts ast)) 10)
      (take (prune 2 ((sxpath '(//)) ast)) 10)
      (andmap string? (filter (not/c list?) ((sxpath '(//)) ast)))
      ((sxpath '(// VarName)) ast)
      (prune 4 ((sxpath '(// TyClD)) ast))
      (prune 3 ((sxpath '(// TyClD SynDecl)) ast))
      (equal? ((sxpath '(// TyClD SynDecl)) ast)
              ((sxpath '(// TyClD / SynDecl)) ast))
      ((sxpath '(// SigD // VarName)) ast)
      ((sxpath '(// (*or* TvName TcClsName))) ast))

(show (group first second (prune 3 ((sxpath "//VarName/..") ast)))
      (group first second (filter (= depth 3) (parts ast))))

#;{(define module (filter (not/c (curry equal? "Nothing"))
                          (untag (find1 'HsModule ast))))
   (define top-kinds '(TyClD SigD ValD))
   (define top (find top-kinds ast))
   (check-equal? (map first module) (map first top))

   (define top-kinds′ (group-by identity (prune 4 top)))
   (check-equal? (length top-kinds) (length top-kinds′))
   (check-equal? (map first top-kinds′)
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