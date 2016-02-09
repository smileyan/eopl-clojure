(ns eopl.ch1.20
  (:require [eopl.common :refer :all]))

; count-occurrences : Sym * ListOf(Sym) -> Int
; usage: (count-occurrences s slist) returns the number of occurrences of s in slist
(defn count-occurrences [s slist]
  (if (empty? slist)
    0
    (if (symbol? (car slist))
      (if (= s (car slist))
        (+ 1 (count-occurrences s (cdr slist)))
        (count-occurrences s (cdr slist)))
      (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist))))))