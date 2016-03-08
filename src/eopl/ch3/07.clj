(add-exp (exp1 exp2)
  (diff-exp (exp1 minus(exp2))))

(mul-exp (exp1 exp2)
        (let [val1 (value-of exp1 env)
              val2 (value-of exp2 env)]
          (let [num1 (expval->num val1)
                num2 (expval->num val2)]
            (num-val (* num1 num2)))))

(div-exp (exp1 exp2)
  (let [val1 (value-of exp1 env)
        val2 (value-of exp2 env)]
    (let [num1 (expval->num val1)
          num2 (expval->num val2)]
      (num-val (quotient num1 num2)))))