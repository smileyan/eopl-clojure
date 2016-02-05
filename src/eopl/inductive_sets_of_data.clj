(ns eopl.inductive-sets-of-data
  (:require [eopl.common :refer :all]))

; in-S? : N -> Bool
; usage: (in-S? n) = #t if n is in S, #f otherwise
(defn in-S? [n]
    (if (zero? n) true
      (if (>= (- n 3) 0)
        (in-S? (- n 3))
        false)))

; list-length : List -> Int
; usage: (list-length l) = the length of l
(defn list-length [lst]
  (if (empty? lst)
    0
    (+ 1 (list-length (rest lst)))))

; nth-element : List * Int -> SchemeVal
; usage: (nth-element lst n) = the n-th element of lst
(defn report-list-too-short [n]
  ())
(defn nth-element [lst n]
  (if (empty? lst)
    (report-list-too-short n)
    (if (zero? n)
      (first lst)
      (nth-element (rest lst) (- n 1)))
      ))

; List-of-Symbol ::= () | (Symbol . List-of-Symbol)

; remove-first : Sym * Listof(Sym)  ->  Listof(Sym)
; usage:  (remove-first s los) returns a list with
;         the same elements arranged in the same
;         order as los, except that the first
;         occurrence of the symbol s is removed.
(defn remove-first [s los]
  (if (empty? los)
    '()
    (if (= (first los) s)
      (rest los)
      (cons (first los) (remove-first s (rest los))))))

; LcExp ::= Identifier
;       ::= (lambda (Identifier) LcExp)
;       ::= (LcExp LcExp)
; occurs-free? : Sym * LcExp -> Bool
; usage: returns #t if the symbol var occurs free
;        in exp, otherwise returns #f.

(defn occurs-free? [var exp]
  (cond
    (symbol? exp) (= var exp)
    (= (car exp) 'lambda)
     (and
       (not (= var (car (cadr exp))))
       (occurs-free? var (caddr exp)))
    :else
      (or 
        (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))

; S-list ::= ({S-exp}*)
; S-exp  ::= Symbol | S-list

; S-list ::= ()
;        ::= (S-exp . S-list)
;  S-exp ::= Symbol | S-list

; subst: Sym * Sym * S-list -> S-list
(declare subst-in-s-exp)
(defn subst [new old slist]
  (if (empty? slist)
    '()
    (cons
      (subst-in-s-exp new old (car slist))
      (subst new old (cdr slist)))))

; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
(defn subst-in-s-exp [new old sexp]
  (if (symbol? sexp)
    (if (= sexp old) new sexp)
    (subst new old sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                              Follow the Grammar!                              ;
; When defining a procedure that operates on inductivly defined data, the       ;
; structure of the program should be patterned after the structure of the data. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; . Write one procedure for each nonterminal in the grammar. The procedure
;   will be responsible for handling the data corresponding to that 
;   nonterminal, and nothing else.

; . In each procedure, write one alternative for each production corresponding
;   to that nonterminal. You may need additional case structure, but this
;   will get you started. For each nonterminal that appears in the right-hand 
;   side, write a recursive call to the procedure for that nonterminal.

; 1.3 Auxiliary Procedures and Context Arguments

; number-elements-from : Listof(SchemeVal) * Int -> Listof(List(Int, SchemeVal))
; usage: (number-elements-from '(v0 v1 v2 ...) n)
;         = ((n v0) (n + 1 v1) (n+2 v2) ...)
(defn number-elements-from [lst n]
  (if (empty? lst)
    '()
    (cons
      (list n (car lst))
      (number-elements-from (cdr lst) (+ n 1)))))

(defn number-elements [lst]
  (number-elements-from lst 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  No Mysterious Auxiliaries!                              ;
; When defining an auxiliary procedure, always specify what it does on all ;
; arguments, not just the initial values.                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list-sum : Listof(Int) -> Int
(defn list-sum [loi]
  (if (empty? loi)
    0
    (+ (car loi)
       (list-sum (cdr loi)))))

; partial-vector-sum : Vectorof(Int) * Int -> Int
; usage: if 0 <= n < length(v), then
;           (partial-vector-sum v n) = sum(vi)(0<= i <=n)

(defn partial-vector-sum [v n]
  (if (empty? n)
    (nth v 0)
    (+ (nth v n)
       (partial-vector-sum v (- n 1)))))