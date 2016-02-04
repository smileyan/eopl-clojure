(ns eopl.ch1.12
  (:require [eopl.common :refer :all]))


(defn subst-inlining [new old slist]
  (if (empty? slist)
    '()
    (cons
      (if (symbol? (car slist))
        (if (= (car slist) old) new (car slist))
        (subst-inlining new old (car slist)))
      (subst-inlining new old (cdr slist)))))