(ns eopl.ch1.28
  (:require [eopl.common :refer :all]))

; merge: Listof(Int) * Listof(Int) -> Listof(Int)
; usage : (merge loi1 loi2), where loi1 and loi2 are lists of Integers
; that are sorted in ascending order, returns a sorted list of all the integers in loi1 and
; loi2.
(declare merge-with-rst)
(defn my-merge [loi1 loi2]
  (merge-with-rst < loi1 loi2 '()))

(defn merge-with-rst [pred loi1 loi2 rst]
  (cond (empty? loi1) (into loi2 rst)
        (empty? loi2) (into loi1 rst)
  :else  (let [f1 (car loi1)
               f2 (car loi2)
               r1 (cdr loi1)
               r2 (cdr loi2)]
           (if (pred f1 f2)
             (merge-with-rst pred r1 loi2 (conj rst f1))
             (merge-with-rst pred loi1 r2 (conj rst f2))))))


(defn my-merge-with-pred [pred loi1 loi2]
  (merge-with-rst pred loi1 loi2 '()))

