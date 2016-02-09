(ns eopl.common)

(defn car [l]
  (first l))

(defn caar [l]
  (car (car l)))

(defn cdr [l]
  (rest l))

(defn cadr [l]
  (first (rest l)))

(defn caddr [l]
  (first (rest (rest l))))
