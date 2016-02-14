(ns eopl.ch2.02
  (:require [eopl.common :refer :all]))

(def N 10)

(defn zero []
  '())

(defn is-zero? [n]
  (empty? n))


(defn successor [n]
  (cond
    (empty? n) '(1))
    (= (+ (car n) 1) N) (cons 0 (successor (cdr n)))
    :else (cons (+ (car n) 1) (cdr n)))

(defn predecessor [n]
  (cond
    (empty? n) '()
    (zero? (car n)) (cons (- N 1) (predecessor (cdr n)))
    (and (= (car n) 1) (empty? (cdr n))) '()
    :else (cons (- (car n) 1) (cdr n))))
; 🐫
; 🍺