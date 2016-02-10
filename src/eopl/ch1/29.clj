(ns eopl.ch1.29
  (:require [eopl.common :refer :all])
  (:require [eopl.ch1.28 :refer :all])
  )

; sort: Listof(Int) -> Listof(Int)
; usage : (sort loi) returns a list of the elements of loi in ascending
; order.
(defn my-sort [loi]
    (let [half (quot (count loi) 2)]
      (if (zero? half)
          loi
          (my-merge (my-sort (take half loi))
                    (my-sort (take-last (- (count loi) half) loi))))))