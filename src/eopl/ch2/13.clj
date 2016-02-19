(ns eopl.ch2.13
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]))

(defn empty-env13 []
  (cons (fn [search-var] report-no-binding-found search-var)
        (list true)))

(defn apply-env13 [env search-var]
  ((car env) search-var))

(defn extend-env13 [saved-var saved-val saved-env]
  (cons (fn [search-var]
          (if (= search-var saved-var)
            saved-val
            (apply-env13 saved-env search-var)))
          (list false)))

(defn empty-env13? [env]
  (cadr env))

(defn e13 []
  (extend-env13 'd 6
     (extend-env13 'y 8
        (extend-env13 'x 7
           (extend-env13 'y 14
              (empty-env13))))))
