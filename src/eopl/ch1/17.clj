(ns eopl.ch1.17
  (:require [eopl.common :refer :all]))
  
; down : List -> Listof(List)
; usage: (down lst) wraps parentheses aourd each top-level element of lst.
(defn down [lst]
  (if (empty? lst)
    '()
    (cons (list (car lst))
          (down (cdr lst)))))