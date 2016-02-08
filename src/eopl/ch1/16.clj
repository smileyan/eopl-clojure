(ns eopl.ch1.16
  (:require [eopl.common :refer :all]))

; invert : ListofList -> Listof(List)
; usage: (invert lst) return a list with each 2-list reversed.

(defn invert [lst]
  (if (empty? lst)
    '()
    (cons (list (cadr (car lst)) 
                (car  (car lst)))    
          (invert (cdr lst)))))
