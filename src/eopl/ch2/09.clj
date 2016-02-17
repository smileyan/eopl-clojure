(ns eopl.ch2.09
  (:require [eopl.common :refer :all]))

(defn has-binding? [env var]
  (cond
    (empty? env) false
    (= (caar env) var) true
    :else (has-binding? (cdr env) var)
    ))
