(ns eopl.ch1.25
  (:require [eopl.common :refer :all]))

; exists?: pred * List -> Boolean
; usage : (exists? pred lst) returns false if any element of lst satisfies
; pred, and returns #f otherwise.
(defn exists? [pred lst]
  (if (empty? lst)
    false
    (if (pred (car lst))
      true
      (exists? pred (cdr lst)))))