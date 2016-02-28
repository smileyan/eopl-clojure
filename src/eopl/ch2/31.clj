Prefix-list ::= (Prefix-exp)
Prefix-exp  ::= Int
            ::= - Prefix-exp Prefix-exp

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)))

(- - 3 2 - 4 - 12 7)

(diff-exp
  (diff-exp
    (const-exp 3)
    (const-exp 2))
  (diff-exp
    (const-exp 4)
    (diff-exp
      (const-exp 12)
      (const-exp 7))))
