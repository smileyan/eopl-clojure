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
        
        Exercise 2.2.1.
            a. 1.2*(2-1/3)+ -8.7
                > (+ (* 1.2 (- 2 (/ 1 3))) -8.7)
            b. (2/3 + 4/9) / (5/11 - 4/3)
                > (/ (+ (/ 2 3) (/ 4 9)) (- (/ 5 11) (/ 4 3)))
        
        Exercise 2.2.2.
        
        Exercise 2.2.3.
            > (cons 'car 'cdr)
            - (car . cdr)
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
        
        occur free or free variable of the lambda expression 
        > (let ([x 'a])
            (let ([f (lambda (y) (list x y))])
              (f 'b)))
        - (a b)
        
        > (let ([f (let ([x 'sam])
                     (lambda (y z) (list x y z)))])
            (f '1 'am))
        - (sam i am)
        
        > (let ([f (let ([x 'sam])
                     (lambda (y z) (list x y z)))])
            (let ([x 'not-sam])
              (f '1 'am)))
        - (sam i am)
        
        (let ([x 'a])) (cons x x) = ((lambda (x) (cons x x)) 'a)
        
        (let ((var expr) ...) body1 body2 ...) is equivalent to ((lambda (var ...) body1 body2 ...)
                                                                  expr ...)
        
        > (let ([f (lambda x x)])
            (f 1 2 3 4))
        - (1 2 3 4)
        
        > (let ([f (lambda x x)])
            (f))
        - ()
        
        > (let ([g (lambda (x . y) (list x y))])
            (g 1 2 3 4))
        - (1 (2 3 4))
        
        > (let ([h (lambda (x y . z) (list x y z))])
            (h 'a 'b 'c 'd))
        - (a b (c d))
        
        Exercise 2.5.1
            a.
              (let ([f (lambda (x) x)])
                (f 'a))
              - 'a
            b.
              (let ([f (lambda x x)])
                (f 'a))
              - '(a)
            c.
              (let ([f (lambda (x . y) x)])
                (f 'a))
              - 'a
            d.
              (let ([f (lambda (x . y) y)])
                (f 'a))
              - '()
        Exercise 2.5.3
            (let ([list (lambda x x)]))

    Section 2.6. Top-Level Definitions
    
        > (define double-any
            (lambda (f x)
              (f x x)))
        > (double-any + 10)
        - 20
        > (double-any cons 'a)
        - (a . a)
        
        > (define sandwich "peanut-butter-and-jelly") 
        > sandwich 
        - "peanut-butter-and-jelly"
        
        > (define xyz '(x y z))
        > (let ([xyz '(z y x)])
            xyz)
        - (z y x)
        
        > (define list (lambda x x))
        
        > (define cadr
            (lambda (x)
              (car (cdr x))))
        > (define cddr
            (lambda (x)
              (cdr (cdr x))))
        > (cadr '(a b c))
        - 'b
        > (cddr '(a b c))
        - '(c)
        
        a proper list of variables
        (define var0
          (lambda (var1 ... varn)
            e1 e2 ...))
        (define (var0 var1 ... varn)
          e1 e2 ...)
        
        a single variable
        (define var0
          (lambda varr
            e1 e2 ...))
        (define (var0 . varr)
          e1 e2 ...)
        
        am improper list of variables
        (define var0
          (lambda (var1 ... varn . varr)
            e1 e2 ...))
        (define (var0 var1 ... varn . varr)
          e1 e2 ...)
        
        (define (cadr x)
          (car (cdr x)))
        (define (list . x) x)
        
        (define doubler
          (lambda (f)
            (lambda (x) (f x x))))
        > (define double (doubler +))
        > (double 13/2)
        - 13
        
        > (define double-cons (doubler cons))
        - (double-cons 'a)
        - (a . a)
        
        > (define double-any
            (lambda (f x)
              ((doubler f) x)))
        > (i-am-not-defined 3)
         i-am-not-defined: undefined;
          cannot reference undefined identifier
        > (define proc1
            (lambda (x y)
              (proc2 y x)))
        > (define proc2 cons)
        > (proc1 'a 'b)
        - '(b . a)
        
        Exercise 2.6.1
        Exercise 2.6.2
        > (define compose
            (lambda (p1 p2 var)
              (p1 (p2 var))))
        > (define cadr
            (lambda (l)
              (compose car cdr l)))
        > (define cddr
            (lambda (l)
              (compose cdr cdr l)))

    Section 2.7. Conditional Expressions
        > (define abs
            (lambda (x)
              (if (< x 0)
                (- 0 x)
                x)))
        > (abs 99)
        - 99
        > (abs -99)
        - 99
        
        (if test consequent alternative)
        > (define abs
            (lambda (n)
              ((if (>= 0 n) + -)
               0
               n)))
        > (define reciprocal
            (lambda (n)
              (if (= 0 n)
                "oops"
                (/ 1 n))))
        > (define reciprocal
            (lambda (n)
              (and (not (= 0 n))
                   (/ 1 n))))
        > (reciprocal 3)
        - 1/3
        > (reciprocal 0.5)
        - 2.0
        > (reciprocal 0)
        - #f
        The procedures =, <, >, <=, and >= are called predicates. 
        > (null? '())
        - #t
        > (null? 'abc)
        - #f
        > (null? '(x y z))
        - #f
        > (null? (cdddr '(x y z)))
        - #t

        > (define lisp-cdr
            (lambda (x)
              (if (null? x)
                  '()
                  (cdr x)))) 

        > (lisp-cdr '(a b c))
        - (b c)
        > (lisp-cdr '(c))
        - ()
        > (lisp-cdr '())
        - ()

        > (eqv? 'a 'a)
        - #t
        > (eqv? 'a 'b)
        - #f
        > (eqv? #f #f)
        - #t
        > (eqv? #t #t)
        - #t
        > (eqv? #f #t)
        - #f
        > (eqv? 3 3)
        - #t
        > (eqv? 3 2)
        - #f
        > (let ([x "Hi Mom!"])
            (eqv? x x))
        - #t
        > (let ([x (cons 'a 'b)])
            (eqv? x x))
        - #t
        > (eqv? (cons 'a 'b) (cons 'a 'b))
        - #f
        
        type predicates: pair?, symbol?, number?, and string?
        > (pair? '(a . c))
        - #t
        > (pair? '(a b c))
        - #t
        > (pair? '())
        - #f
        > (pair? 'abc)
        - #f
        > (pair? "Hi Mom!")
        - #f
        > (pair? 1234567890)
        - #f
        > (define reciprocal
            (lambda (n)
              (if (and (number? n) (not (= 0 n)))
                (/ 1 n)
                "oops!")))
        > (reciprocal 2/3)
        - 3/2
        > (reciprocal 'a)
        - "oops!"
        > (define reciprocal
            (lambda (n)
              (if (and (number? n) (not (= n 0)))
                (/ 1 n)
                (assertion-violation 'reciprocal
                  "improper argument"
                  n))))
        > (reciprocal .25)
        - 4.0
        > (reciprocal 0)
        - exception in reciprocal: improper argument 0
        > (reciprocal 'a)
        - exception in reciprocal: improper argument a
        > (define sign
            (lambda (n)
              (if (< n 0)
                -1
                (if (= n 0)
                  0
                  +1))))
        > (sign -88.3)
        - -1
        > (sign 0)
        - 0
        > (sign 333333333333)
        - 1
        > (* (sign -88.3) (abs -88.3))
        - -88.3
        > (define sign
            (lambda (n)
              (cond
                [(< n 0) -1]
                [(> n 0) 1]
                [else 0])))
    Section 2.8. Simple Recursion
        (define goodbye
          (lambda ()
            (goodbye)))
        (define length
          (lambda (ls)
            (if (null? ls)
              0
              (+ 1 (length (cdr ls))))))
        (length '())
         0
        (length '(a))
         1
        (length '(a b))
         2
        (length '(a b c d))
         |(length (a b c d))
         | (length (b c d))
         | |(length (c d))
         | | (length (d))
         | | |(length ())
         | | |0
         | | 1
         | |2
         | 3
         |4
        (define list-copy
          (lambda (ls)
            (if (null? ls)
              '()
              (cons (car ls) 
                (list-copy (cdr ls))))))
        (define memv
          (lambda (x ls)
            (cond
              [(null? ls) #f]
              [(eqv? (car ls) x) ls]
              [else (memv x (cdr ls))])))
        > (memv 'a '(a b b d))
        - (a b b d)
        > (memv 'b '(a b b d))
        - (b b d)
        > (memv 'c '(a b b d))
        - #f
        > (memv 'd '(a b b d))
        - (d)
        > (if (memv 'b '(a b b d))
              "yes"
              "no")
        - "yes"
        > (define remv
            (lambda (x ls)
              (cond
                [(null? ls) '()]
                [(eqv? x (car ls)) (memv x (cdr ls))]
                [else (cons (car ls) (memv x (cdr ls)))])))
        > (define tree-copy
            (lambda (tr)
              (if (pair? tr)
                (cons (tree-copy (car tr))
                      (tree-copy (cdr tr)))
                tr)))
        > (tree-copy '((a . b) . c))
        - ((a . b) . c)
        > (define abs-all
            (lambda (ls)
              (if (null? ls)
                '()
                (cons (abs (car ls)
                      (abs-all (cdr ls)))))))
        > abs-all '(1 -2 3 -4 5 -6))
        - (1 2 3 4 5 6)
    Section 2.9. Assignment
        > (define abcde '(a b c d e))
        > abcde
         - (a b c d e)
        > (set! abcde (cdr abcde))
        > abcde
         - (b c d e)
        > (let ([abcde '(a b c d e)])
            (set! abcde (reverse abcde))
            abcde)
         - (e d c b a)
        > (define quadratic-formula
            (lambda (a b c)
              (let ([root1 0] [root2 0] [minusb 0] [radical 0] [divisor 0])
                (set! minusb (- 0 b))
                (set! radical (sqrt (- (* b b) (* 4 (* a c)))))
                (set! divisor (* 2 a))
                (set! root1 (/ (+ minusb radical) divisor))
                (set! root2 (/ (- minusb radical) divisor))
                (cons root1 root2))))
        > (quadratic-formula 2 -4 -6)
         - (3 . -1)
        > (define quadratic-formula
            (lambda (a b c)
              (let ([minusb (- 0 b)]
                    [radical (sqrt (- (* b b) (* 4 (* a c))))]
                    [divisor (* 2 a)])
                (let ([root1 (/ (+ minusb radical) divisor)]
                      [root2 (/ (- minusb radical) divisor)])
                  (cons root1 root2)))))
        > (define kons-count 0)
        > (define kons
            (lambda (x y)
              (set! kons-count (+ kons-count 1))
              (cons x y)))
        > (define next 0)
        > (define count
            (lambda ()
              (let ([v next])
                (set! next (+ next 1))
                v)))
        > (define count
            (let ([next 0])
              (lambda ()
                (let ([v next])
                  (set! next (+ next 1))
                  v))))
        > (define make-counter
            (lambda ()
              (let ([next 0])
                (lambda ()
                  (let ([v next])
                    (set! next (+ next 1))
                    v)))))
        > (define count1 (make-counter))
        > (define count2 (make-counter)) 
        > (count1)
         - 0
        > (count2)
         - 0
        > (count1)
         - 1
        > (count1)
         - 2
        > (count2)
         - 1
        > (define shhh #f)
        > (define tell #f)
        > (let ([secret 0])
            (set! shhh
              (lambda (message)
                (set! secret message)))
            (set! tell
              (lambda ()
                secret))) 

        > (shhh "sally likes harry")
        > (tell)
         - "sally likes harry"
        > secret
         - exception: variable secret is not bound
        > (define lazy
            (lambda (t)
              (let ([val #f] [flag #f])
                (lambda ()
                  (if (not flag) //the alternative subexpression of an if expression can be omitted
                    (begin (set! val (t))
                           (set! flag #t)))
                  val))))
        > (define p
            (lazy (lambda ()
                    (display "Ouch!")
                    (newline)
                    "got me")))
        > (define make-stack
            (lambda ()
              (let ([ls '()])
                (lambda (message . args)
                  (cond
                    [(eqv? message 'empty?) (null? ls)]
                    [(eqv? message 'push!) (set! ls (cons (car args) ls))]
                    [(eqv? message 'top) (car ls)]
                    [(eqv? message 'pop!) (set! ls (cdr ls))]
                    [else "oops"])))))
        > (define stack1 (make-stack))
        > (define stack2 (make-stack))
        > (list (stack1 'empty?) (stack2 'empty?))
         - (#t #t) 

        > (stack1 'push! 'a)
        > (list (stack1 'empty?) (stack2 'empty?))
         - (#f #t) 

        > (stack1 'push! 'b)
        > (stack2 'push! 'c)
        > (stack1 'top)
         - b
        > (stack2 'top)
         - c 

        > (stack1 'pop!)
        > (stack1 'top)
         - a
        > (list (stack1 'empty?) (stack2 'empty?))
         - (#f #f) 

        > (stack1 'pop!)
        > (list (stack1 'empty?) (stack2 'empty?))
         - (#t #f)
        > (define p (list 1 2 3))
        > (set-car! (cdr p) 'two)
        > p
         - (1 two 3)
        > (set-cdr! p '())
        > p
         - (1)

