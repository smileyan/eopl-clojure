2. Getting Started

    Section 2.1. Interacting with Scheme
        read-evaluate-print
        
        > "Hi Mom!"
        - "Hi Mom!"
        > +
        - #<procedure>
        
        > (car '(a b c))
        - a
        > (cdr '(a b c))
        - (b c)
        > (cons 'a '(b c))
        - (a b c)
        > (cons (car '(a b c))
                (cdr '(d e f)))
        - (a e f)
        
        > (define square
            (lambda (n)
              (* n n)))
        
        > (square 5)
        - 25
        > (square -200)
        - 40000
        > (square 0.5)
        - 0.25
        > (square -1/2)
        - 1/4
        
        in reciprocal.ss
        (define reciprocal
          (lambda (n)
            (if (= n 0)
              "oops!"
              (/ 1 n))))
        > (load "reciprocal.ss")
        > (reciprocal 10)
        - 1/10
        > (reciprocal 1/10)
        - 10
        > (reciprocal 0)
        - "oops!"
        > (reciprocal (reciprocal 1/10))
        - 1/10
    
    Section 2.2. Simple Expressions
        constant data objects: strings,numbers,symbols,and lists.
        
        > (quote (1 2 3 4 5))
        - (1 2 3 4 5)
        > '(1 2 3 4)
        - (1 2 3 4)
        > (quote hello)
        - hello
        > '2
        - 2
        > '2/3
        - 2/3
        
        > (cons 'a 'b)
        - (a . b)
        > (cdr '(a .b))
        - b
        > (cons 'a '(b . c))
        - (a b . c)
        > '(a . (b . (c . ())))
        - '(a b c)
        > (list 'a 'b 'c)
        - (a b c)
        > (list 'a)
        - (a)
        > (list)
        - ()
    Section 2.3. Evaluating Scheme Expressions
        A Scheme evaluator is free to evaluate the expressions in any order.
        > ((car (list + - * /)) 2 3)
    Section 2.4. Variables and Let Expressions
        > (let ((x 2))
            (+ x 3))
        - 5
        > (let ((y 3))
            (+ 2 y))
        - 5
        > (let ((x 2) (y 3))
            (+ x y))
        - 5
        
        (let ((var expr) ...) body1 body2 ...)
        (+ (* 4 4) (* 4 4)) => 32
        (let ((a (* 4 4))) (+ a a)) => 32
        
        > (let ([list1 '(a b c)] [list2 '(d e f)])
            (cons (cons (car list1)
                        (car list2))
                  (cons (car (cdr list1))
                        (car (cdr list2)))))
        - ((a . d) b . e)
        
        > (let ([f +])
            (f 2 3))
        - 5
        
        > (let ([f +] [x 2])
            (f x 3))
        - 5
        
        > (let ([f +] [x 2] [y 3])
            (f x y))
        - 5
        
        > (let ([a 4] [b -3])
            (let ([a-squared (* a a)]
                  [b-squared (* b b)])
              (+ a-squared b-squared)))
        - 25
        
        > (let ([x 1])
            (let ([x (+ x 1)])
              (+ x x)))
        - 4

    Section 2.5. Lambda Expressions

        (lambda (x) (+ x x)) #<procedure>
        (lambda (var ...) body1 body2 ...)
        
        > ((lambda (x) (+ x x)) (* 3 4))
        - 24
        
        > (let ([double (lambda (x) (+ x x))])
            (list (double (* 3 4))
                  (double (/ 99 11))
                  (double (- 2 7))))
        - (24 18 -10)
        
        > (let ([double-cons (lambda (x) (cons x x))])
            (double-cons 'a))
        - (a . a)
        
        > (let ([double-any (lambda (f x) (f x x))])
            (list (double-any + 13)
                  (double-any cons 'a)))
        - (26 (a . a))
