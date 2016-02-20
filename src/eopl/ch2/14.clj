(ns eopl.ch2.14
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]
            [eopl.ch2.13]))

(defn has-binding? [env search-var]
  ((cadr env) search-var))

