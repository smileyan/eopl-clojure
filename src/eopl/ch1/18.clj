(ns eopl.ch1.18
  (:require [eopl.common :refer :all]))

; swapper: Sym * Sym * List -> Listof(Sym)
; usage: (swapper s1 s2 slist) returns a list the same as slist, 
; but with all occurences of s1 replaced by s2 and all occurences of s2 replaced by s1. 
(defn swapper [s1 s2 slist]
  (if (empty? slist)
    '()
    (if (symbol? (car slist))
      (cond
        (= s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist)))
        (= s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist)))
        :else (cons (car slist) (swapper s1 s2 (cdr slist))))
      (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))
    ))