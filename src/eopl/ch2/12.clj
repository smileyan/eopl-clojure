(ns eopl.ch2.12
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]))

(declare report-empty-stack)

(defn empty-stack []
  (fn [cmd]
    (cond
      (= cmd 'top) (report-empty-stack 'top)
      (= cmd 'pop) (report-empty-stack 'pop)
      :else (report-empty-stack 'unknown))))

(defn push [saved-stack var]
  (fn [cmd]
    (cond
      (= cmd 'top) var
      (= cmd 'pop) saved-stack
      :else (report-empty-stack 'unknown))))

(defn pop [stack]
  (stack 'pop))

(defn top [stack]
  (stack 'top))

(defn report-empty-stack [func]
    "Empty stack")


(defn s [] (empty-stack))
(defn x1 [] (push (s) 1))
(defn x2 [] (push (x1) 2))
(defn x3 [] (push (x2) 3))
