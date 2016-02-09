(ns eopl.ch1.23
  (:require [eopl.common :refer :all]))

; list-index : pred * Listof(sym) -> Int or False
; usage: (list-index pred lst) returns the 0-based position of the
; first element of lst that satisfies the predicate pred. If no element of lst satisfies
; the predicate, then list-index returns #f.
(declare list-index-nth)
(defn list-index [pred lst]
  (list-index-nth pred 0 lst))

(defn list-index-nth [pred n lst]
  (if (empty? lst)
    false
    (if (pred (car lst))
      n
      (list-index-nth pred (+ n 1) (cdr lst)))))