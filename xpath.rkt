#lang racket #| XPath |#

#| Core XPath

 Defined in 2002:
   “Efficient Algorithms for Processing XPath Queries”
   Georg Gottlob, Christoph Koch, and Reinhard Pichler

 Consider unranked, ordered, and labeled tree.

 dom := set of nodes

 firstchild, nextsibling : dom → dom ∪ {null}
 And inverses, to dom ∪ {null} as well.
 Overload f to also refer to the relation {⟨x,f(x)⟩ | x ∈ dom, f(x) ≠ null}

 Σ is a finite labeling alphabet
 T : (Σ ∪ {∗}) → 2^dom
   represents node tests
   T(∗) := dom

 x <doc y (for two nodes x, y ∈ dom) iff the opening tag of x precedes the opening tag of y
 first_{<doc} returns the first node in a set

 child := firstchild.nextsibling∗
 parent := (nextsibling−1)∗.firstchild−1
 descendant := firstchild.(firstchild ∪ nextsibling)∗
 ancestor := (firstchild−1 ∪ nextsibling−1)∗.firstchild−1
 descendant-or-self := descendant ∪ self
 ancestor-or-self := ancestor ∪ self
 following := ancestor-or-self.nextsibling.nextsibling∗ .descendant-or-self
 preceding := ancestor-or-self.nextsibling−1.(nextsibling−1)∗.descendant-or-self
 following-sibling := nextsibling.nextsibling∗
 preceding-sibling := (nextsibling−1)∗.nextsibling−1 |#

; child: /
; descendant: //
; 

; Core, Simple, Navigational

#| “A Trace Semantics for Positive Core XPath”
  Pieter Hartel, Univ. of Twente, http://www.cs.utwente.nl/~pieter

 [From the paper:]
 Full XPath is impractical to use as a tool for investigating the fundamental relation
  between query and access control.
 Several subsets have been defined, such as
  Core XPath [16], Simple XPath [2], and Navigational XPath [26].
 We adopt a similar approach in that we omit expressions and focus on location paths and predicates.
 Contrary to some of the work cited earlier, we do support most (11 of the 13) axes,
  omitting attribute and namespace only.
 We omit negations for reasons to be explained later.
 Our subset is essentially positive core XPath, which is core XPath [16] without negations.
 The abstract syntax of positive core XPath is:
 X ≡ X | X |/X | X/X | X[Q] |A :: L
 Q ≡X
 A ≡ self |
     child | descendant | descendant or self |
     parent | ancestor | ancestor or self |
     preceding sibling | following sibling | preceding | following
 The node test in a step is restricted to a name test (i.e. kind tests are not supported,
  which is consistent with the use of undecorated XML data).
 We use location paths X by way of predicates Q. |#
