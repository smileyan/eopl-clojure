(ns eopl.ch2.15
  (:require [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]))

(defn var-exp [var]
  var)

(defn lambda-exp [bound-var body]
  (list 'lambda (list bound-var) body))

(defn app-exp [exp1 exp2]
  (list exp1 exp2))

(defn var-exp? [exp]
  (symbol? exp))

(defn lambda-exp? [exp]
  (and (list? exp)
       (= 'lambda (car exp))))

(defn app-exp? [exp]
  (and (list? exp)
       (list? (cdr exp))
       (empty? (cddr exp))))

(defn var-exp->var [exp] exp)
(defn lambda-exp->bound-var [exp] (caadr exp))
(defn lambda-exp->body [exp] (caddr exp))
(defn app-exp->rator [exp] (car exp))
(defn app-exp->rand [exp] (cadr exp))