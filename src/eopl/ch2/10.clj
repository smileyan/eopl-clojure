(ns eopl.ch2.10
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]))

; usage : (extend-env* (var1 ... varn) (val1 ... valn) [f]) = [g],
;            where g(var) = vali if var = vari for some i such that 1≤i≤k
;                           f(var) otherwise

(defn extend-env* [lofvar lofval env]
  (if (empty? lofvar)
    env
    (let [var (car lofvar)
          val (car lofval)]
      (recur (cdr lofvar)
             (cdr lofval)
             (extend-env var val env)))))

