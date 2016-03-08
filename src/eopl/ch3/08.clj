 (equal?-exp (exp1 exp2)
   (let [val1 (value-of exp1 env)
         val2 (value-of exp2 env)]
     (let [num1 (expval->num val1)
           num2 (expval->num val2)]
       (bool-val (= num1 num2)))))

(greater?-exp (exp1 exp2)
  (let [val1 (value-of exp1 env)
        val2 (value-of exp2 env)]
    (let [num1 (expval->num val1)
          num2 (expval->num val2)]
      (bool-val (> num1 num2)))))
      
(less?-exp (exp1 exp2)
  (let [val1 (value-of exp1 env)
        val2 (value-of exp2 env)]
    (let [num1 (expval->num val1)
          num2 (expval->num val2)]
      (bool-val (< num1 num2)))))