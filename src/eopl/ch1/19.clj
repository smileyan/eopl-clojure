(ns eopl.ch1.19
  (:require [eopl.common :refer :all]))

; list-set : List * Int * Sym -> List
; usage: (list-set lst n x) return a list like lst, except that the n-th 
; element, using zero-based indexing, is x.

(defn list-set [lst n x]
  (cond 
    (empty? lst) (str "")
    (zero? n) (cons x (cdr lst))
    :else (cons (car lst)
                (list-set (cdr lst) (- n 1) x))))
