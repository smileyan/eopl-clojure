(ns eopl.ch1.33
  (:require [eopl.common :refer :all]
            [eopl.ch1.31 :refer :all]
            ))

(declare red-depth)
(defn mark-leaves-with-red-depth [tree]
  (red-depth tree 0))

(defn red-depth [tree n]
  (if (leaf? tree)
    (leaf n)
    (if (= 'red (contents-of tree))
      (interior-node 'red
                     (red-depth (lson tree) (+ 1 n))
                     (red-depth (rson tree) (+ 1 n)))
      (interior-node (contents-of tree)
                     (red-depth (lson tree) n)
                     (red-depth (rson tree) n)))))
