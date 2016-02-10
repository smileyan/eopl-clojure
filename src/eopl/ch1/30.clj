(ns eopl.ch1.30
  (:require [eopl.common :refer :all]
            [eopl.ch1.28 :refer :all]))

; sort: Pred * Listof(Int) -> Listof(Int)
; usage : (sort pred loi) returns a list of elements sorted by the predicate.

(defn my-sort->predicate [pred loi]
    (let [half (quot (count loi) 2)]
      (if (zero? half)
          loi
          (my-merge-with-pred pred (my-sort->predicate pred (take half loi))
                    (my-sort->predicate pred (take-last (- (count loi) half) loi))))))
