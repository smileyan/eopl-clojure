(ns eopl.ch1.32
  (:require [eopl.common :refer :all]
            [eopl.ch1.31 :refer :all]
            ))

(defn double-tree [tree]
  (if (leaf? tree)
    (leaf (* 2 (contents-of tree)))
    (interior-node (contents-of tree)
                   (double-tree (lson tree))
                   (double-tree (rson tree)))))
