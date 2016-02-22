(ns eopl.ch2.18
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]))

(defn number->sequence [n]
  (list n '() '()))

(defn make->sequence [n l r]
  (list n l r))

(defn current-element [s]
  (car s))

(defn l-s [s]
  (cadr s))

(defn r-s [s]
  (caddr))

(defn move-to-left [s]
  (let [n (current-element s)
        l (l-s s)
        r (r-s s)]
    (let [n1 (car l)
         l1 (cdr l)
         r1 (cons n r)]
      (make->sequence n1 l1 r1))))

(defn move-to-right [s]
  (let [n (current-element s)
        l (l-s s)
        r (r-s s)]
    (let [n1 (car r)
         l1 (cons n l)
         r1 (cdr r)]
      (make->sequence n1 l1 r1))))

(defn insert-to-left [n s]
  (let [n (current-element s)
        l (cons n (l-s s))
        r (r-s s)]
    (make->sequence n l r)))

(defn insert-to-right [n s]
  (let [n (current-element s)
        l (l-s s)
        r (cons n (r-s s))]
    (make->sequence n l r)))

(defn at-left-end? [s] 
  (empty? (l-s s)))

(defn at-right-end? [s]
  (empty? (r-s s)))
