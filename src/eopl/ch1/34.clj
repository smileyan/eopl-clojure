(ns eopl.ch1.34
  (:require [eopl.common :refer :all]
            ))

(defn path [n bst]
  (cond
    (empty? bst) '()
    (< n (car bst)) (cons 'left (path n (cadr bst)))
    (> n (car bst)) (cons 'right (path n (caddr bst)))
    (= n (car bst)) '()))