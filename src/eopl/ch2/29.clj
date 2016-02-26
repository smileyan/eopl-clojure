; Lc-exp ::= Identifier
;            var-exp (var)
;        ::= (lambda ({Identifier}*) Lc-exp)
;            lambda-exp (bound-vars body)
;        ::= (Lc-exp {Lc-exp}*)
;            app-exp (rator rands)

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-vars (list-of identifier?))
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rands (list-of lc-exp?))))

(defn identifier? [symbol]
  (not (and (symbol? symbol)
            (= symbol 'lambda)))))

(defn list-of [pred]
  (fn [val]
    (or (empty? val)
        (and (parse? val)
             (pred (car val))
             ((list-of pred) (cdr val))))))


=====================
(define parse-expression
  (lambda (datum)
    (cond ((symbol? datum)
           (var-exp datum))
          ((pair? datum)
           (if (eqv? (car datum) 'lambda)
               (lambda-exp (cadr datum)
                           (parse-expression (caddr datum)))
               (app-exp (parse-expression (car datum))
                        (map parse-expression (cdr datum)))))
          (else
           (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression "Syntax error: ~s" datum)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse
  (lambda (exp)
    (cond
     ((eqv? exp 'lambda)
      (error 'parse "lambda is not a valid id"))
     ((symbol? exp)
      (var-expr exp))
     ((and (pair? exp)
	   (eqv? (car exp) 'lambda))
      (lambda-expr (cadr exp) (parse (caddr exp))))
     ((pair? exp)
      (app-expr (parse (car exp))
		(map parse (cdr exp))))
     (else
      (error 'parse "parse error")))))