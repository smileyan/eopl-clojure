(ns eopl.ch2.11
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]
            [eopl.ch2.10 :refer :all]))

(defn extend-env-11 [var val env]
  (extend-env* (list var) (list val) env))

(defn apply-current [vars vals search-var]
  (if (empty? vars)
    (list false)
    (if (= (car vars) search-var)
      (list true (car vals))
      (recur (cdr vars) (cdr vals) search-var))))

(defn apply-env-11 [env search-var]
  (if (empty? env)
    (report-no-binding-found search-var)
    (let [val (apply-current (caar env) (cadar env) search-var)]
      (if (car val)
        (cdr val)
        (recur (cdr env) search-var)))))





