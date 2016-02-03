(ns eopl.inductive-sets-of-data)

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