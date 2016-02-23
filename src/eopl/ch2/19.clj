(ns eopl.ch2.19
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]))


(defn leaf []
  '())
 
(defn make-bintree [i l r]
  (list i l r))

(defn number->bintree [n]
  (make-bintree n (leaf) (leaf)))

(defn current-element-bt [bintree]
  (car bintree))

(defn move-to-left-son [bintree]
  (cadr bintree))

(defn move-to-right-son [bintree]
  (caddr bintree))

(defn at-leaf? [bintree]
  (empty? bintree))

(defn insert-to-left-bt [n bintree]
  (make-bintree 
    (current-element-bt bintree)
    (make-bintree n (move-to-left-son bintree) (leaf))
    (move-to-right-son bintree)
    ))

(defn insert-to-right-bt [n bintree]
  (make-bintree 
    (current-element-bt bintree)
    (move-to-left-son bintree)
    (make-bintree n (leaf) (move-to-right-son bintree))
    ))

(defn t1 []
  (insert-to-right-bt 14
    (insert-to-left-bt 12
      (number->bintree 13))))
