(ns eopl.ch1.24
  (:require [eopl.common :refer :all]))

; every? : pred * List -> Boolean
; usage : (every? pred lst) returns #f if any element of lst fails to
; satisfy pred, and returns #t otherwise.
(defn my-every? [pred lst]
  (if (empty? lst)
    true
    (if (pred (car lst))
      (my-every? pred (cdr lst))
      false)))