(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(defn identifier? [x]
  (and 
    (symbol? x)
    (not= 'lambda x)))

(defn parse-expression [datum]
  (cond
    (symbol? datum) (var-exp datum)
    (pair? datum)
      (if (= (car datum) 'lambda)
        (lambda-exp (car (cadr datum))
                    (parse-expression (caddr datum)))
        (app-exp (parse-expression (car datum))
                 (parse-expression (cadr datum))))
    :else (report-invalid-concrete-syntax datum)))

(defn report-invalid-concrete-syntax [datum]
  (str "syntax error" datum))

(defn unparse-lc-exp [exp]
  (cases lc-exp exp
    (var-exp (var) var)
    (lambda-exp (bound-var body)
      (str "(lambda (" bound-var ")"))
    (app-exp (rator rand)
      (str "c" (unparse-lc-exp rator) " " (unparse-lc-exp rand) "c"))))
