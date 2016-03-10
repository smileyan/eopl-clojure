(define grammar
  '((program (expression) a-program)
  (expression ("list" "(" (separated-list expression ",") ")") list-exp)))

(define-datatype expression expression?
  (list-exp
   (exps (list-of expression?))))

(define list-val [exps]
    (if (null? exps)
	(emptylist-val)
	(pair-val (car exps)
		  (list-val (cdr exps))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (list-exp (args)
		     (list-val (map (apply-elm env) args)))
	   )))