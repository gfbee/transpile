#lang racket

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

(require json)
(define p (open-input-file "Stack.json" #;"MaybeInt.json"))
(read-line p)
(define j (read-json p))

(define (remove key j)
  (cond [(hash? j) (list (for/hash ([(k v) (in-dict j)] #:unless (equal? k key))
                           (values k (remove key v))))]
        [(list? j) (append-map (curry remove key) j)]
        [(equal? j key) (list)]
        [else (list j)]))

(define (collapse j)
  (cond [(hash? j) (define result (for/list ([(k v) (in-dict j)]) (list k (collapse v))))
                   (if (= (length result) 1) (first result) result)]
        [(list? j) (define result (map collapse j))
                   (if (= (length result) 1) (first result) result)]
        [else j]))

(define ast (collapse (remove "PlaceHolder" (remove 'location j))))

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
   (define results (find 'GRHS ast))
   }#;(map list fs patterns results)

(define (unparse . ast) (apply ~a (add-between (flatten ast) " ")))
#;(unparse f ':: (add-between f-type '->))
#;(unparse 'data data '= (add-between constructors '\|) 'deriving deriving)


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

#| Generalized. |#

(require rackunit)

; Leaf data
; ---------
(define leaves (flatten ast))
; racket/list : group-by remove-duplicates check-duplicates
; See also: sets, statistics.
;
; It's only symbols and strings.
(sort (remove-duplicates (filter symbol? leaves)) symbol<?)
(sort (remove-duplicates (filter string? leaves)) string<?)
(check-pred empty? (filter (not/c (or/c symbol? string?)) leaves))

(require "tree-query.rkt")

; Compound tags.
(filter list? (tags ast))
; Non-symbol/list tags [all would be strings, by above].
(filter (not/c (or/c symbol? list?)) (tags ast))

(define symbol string->symbol)
(define (from-singleton l) (match l [`(,e) e]))
(define (untag pair) (match pair [`(,_ ,e) e]))
(define find1 (compose from-singleton find))

(prune 7 ast)
(define module (filter (not/c (curry equal? "Nothing"))
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

(map SigD signatures)
