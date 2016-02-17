(ns eopl.ch2.07
  (:require [eopl.common :refer :all]))

; apply-env : Env * Var -> SchemeVal
(defn apply-env-07 [env search-var]
  (cond
    (= (car env) 'empty-env) (report-no-binding-found search-var)
    (not= (car env) 'extend-env) (report-invalid-env env)
    :else (let [saved-var (cadr env)
                saved-val (caddr env)
                saved-env (cadddr env)]
            (if (= search-var saved-var)
              saved-val
              (recur saved-env search-var)))))
