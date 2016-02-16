(ns eopl.common)

(defn car [l]
  (first l))

(defn cdr [l]
  (rest l))

(defn caar [l]
  (car (car l)))

(defn cdar [l]
  (car (cdr (car l))))

(defn cadr [l]
  (first (rest l)))

(defn cddr [l]
  (rest (rest l)))

(defn caddr [l]
  (first (rest (rest l))))

(defn cadddr [l]
  (first (rest (rest (rest l)))))

(defn report-no-binding-found [search-var]
  (str 'apply-env))

(defn report-invalid-env [env]
  (str 'apply-env))
