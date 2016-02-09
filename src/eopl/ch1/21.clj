(ns eopl.ch1.21
  (:require [eopl.common :refer :all]))

; product : List * List -> List
; usage: (product sos1 sos1), where sos1 and sos2 are each a list 
; of symbols without repetitons, returns a list of 2-lists that represents the Cartesian
; product of sos1 and sos2. The 2-lists may appear in any order.
(declare build)
(defn product [sos1 sos2]
  (if (empty? sos1)
    '()
    (cons (build (car sos1) sos2) (product (cdr sos1) sos2))))

(defn build [a lst]
  (if (empty? lst)
    '()
    (cons (list a (car lst)) (build a (cdr lst)))))
