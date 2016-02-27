#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.30

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-vars (list-of identifier?))
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rands (list-of lc-exp?))))
(defn identifier? [x]
  (and (symbol? x)
    (not= 'lambda x)))

(defn parse-expression [datum]
  (cond
    (symbol? datum) (var-exp datum)
    (pair? datum)
      (if (= (car datum) 'lambda)
        (cond 
          (not (pair? (cdr datum)))
            (syntax-error datum "missing a list of bound var")
          (not ((list-of identifier?) (cadr datum)))
            (syntax-error datum "not a list of bound variables")
          (not (pair? (cddr datum)))
            (syntax-error datum "missing a body")
          :else
            (lambda-exp (cadr datum) (parse-expression (caddr datum))))
        (if (not (list? datum))
          (syntax-error datum "not an application expression")
          (app-exp (parse-expression (car datum))
            (map parse-expression (cdr datum)))))
    :else
      (syntax-error datum "not a lambda expression")))



(defn syntax-error [datum message-suffix]
  (str 'parse-expression "Syntax error: ~s is " message-suffix datum))
