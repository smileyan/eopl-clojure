(ns eopl.ch2.05
  (:require [eopl.common :refer :all]))

(defn empty-env-5 []
  '())

(defn extend-env-5 [var val env]
  (cons (cons var (cons val nil)) env))

(defn apply-env-5 [initial-env search-var]
  (loop [env initial-env]
    (cond
      (empty? env) (report-no-binding-found search-var initial-env)
      (not (and (seq? env) (seq? (car env))))
        (report-invalid-env initial-env)
      :else
        (let [saved-var (caar env)
              saved-val (cadar env)
              saved-env (cdr env)]
          (if (= search-var saved-var)
            saved-val
            (recur saved-env))))))

(defn e []
  (extend-env-5 'd 6
    (extend-env-5 'y 8
       (extend-env-5 'x 7
	     (extend-env-5 'y 14
	       (empty-env-5))))))
