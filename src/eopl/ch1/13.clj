(ns eopl.ch1.13
  (:require [eopl.common :refer :all]))

(declare subst-in-sexp)
(defn subst-map [new old slist]
  (map (fn [sexp] (subst-in-sexp new old sexp)) slist))

(defn subst-in-sexp [new old sexp]
  (if (symbol? sexp)
    (if (= sexp old) new sexp)
    (subst-map new old sexp)))