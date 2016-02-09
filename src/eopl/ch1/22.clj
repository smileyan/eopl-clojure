(ns eopl.ch1.22
  (:require [eopl.common :refer :all]))

; filter-in : predicate * List -> List
; usage: (filter-in pred lst) returns the list of those elements in
; lst that satisfy the predicate pred.
(defn filter-in [pred lst]
  (if (empty? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter-in pred (cdr lst)))
      (filter-in pred (cdr lst)))))