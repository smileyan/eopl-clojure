(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("+" "(" expression "," expression ")") plus-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype expression expression?
  (var-exp
    (id symbol?))
  (const-exp
    (num number?))
  (zero?-exp
    (expr expression?))
  (equal?-exp
    (exp1 expression?)
    (exp2 expression?))
  (less?-exp
    (exp1 expression?)
    (exp2 expression?))
  (greater?-exp
    (exp1 expression?)
    (exp2 expression?))
  (if-exp
    (predicate-exp expression?)
    (true-exp expression?)
    (false-exp expression?))
  (minus-exp
    (body-exp expression?))
  (diff-exp
    (exp1 expression?)
    (exp2 expression?))
  (add-exp
    (exp1 expression?)
    (exp2 expression?))
  (mult-exp
    (exp1 expression?)
    (exp2 expression?))
  (div-exp
    (exp1 expression?)
    (exp2 expression?))
  (let-exp
    (var symbol?)
    (value expression?)
    (body expression?))
  (emptylist-exp)
  (cons-exp
    (exp1 expression?)
    (exp2 expression?))
  (car-exp
    (body expression?))
  (cdr-exp
    (body expression?))
  (null?-exp
    (body expression?)))

(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (pair-val
    (car expval?)
    (cdr expval?))
  (emptylist-val))

;;; extractors:

(defn expval->pair [v]
  (cases expval v
	(pair-val (car cdr)
	  (cons car cdr))
    :else (expval-extractor-error 'pair v)))

(defn expval-car [v]
  (cases expval v
    (pair-val (car cdr) car)
	:else (expval-extractor-error 'car v)))

(defn expval-cdr [v]
  (cases expval v
    (pair-val (car cdr) cdr)
	:else (expval-extractor-error 'cdr v)))

(define expval-null? [v]
  (cases expval v
    (emptylist-val () (bool-val #t))
    :else (bool-val #f)))

(emptylist-exp ()
  (emptylist-val))
(cons-exp (exp1 exp2)
  (let [val1 (value-of exp1 env)
		val2 (value-of exp2 env)]
    (pair-val val1 val2)))
(car-exp (body)
  (expval-car (value-of body env)))
(cdr-exp (body)
  (expval-cdr (value-of body env)))
(null?-exp (exp)
  (expval-null? (value-of exp env)))