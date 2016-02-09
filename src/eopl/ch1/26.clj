(ns eopl.ch1.26
  (:require [eopl.common :refer :all]))

; up: List -> List
; usage : (up lst) removes a pair of parentheses from each top-level ele-
; ment of lst. If a top-level element is not a list, it is included in the result, as is.
; The value of (up (down lst)) is equivalent to lst. but (down (up lst)) is 
; not necessarily lst. (See exercise 1.17.)
(defn up [lst]
  (if (empty? lst)
    '()
    (if (seq? (car lst))
      (conj (up (cdr lst))
            (cadr (car lst))
            (caar lst))
      (cons (car lst)
            (up (cdr lst))))))