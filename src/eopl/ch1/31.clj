(ns eopl.ch1.31
  (:require [eopl.common :refer :all]))

; Bintree ::= Int | (Symbol Bintree Bintree)
; cons of bintree
(defstruct bintree :key :lson :rson )

(defn leaf [key]
  (struct bintree key nil nil))

(defn interior-node [key lson rson]
  (struct bintree key lson rson))

(defn leaf? [tree]
  (and (nil? (:lson tree)) (nil? (:rson tree))))

(defn lson [tree]
  (:lson tree))

(defn rson [tree]
  (:rson tree))

(defn contents-of [tree]
  (:key tree))