Chapter 5. Control Operations
    intr
        This chapter introduces the syntactic forms and procedures that serve as control structures for Scheme programs, 
        The first section covers the most basic control structure, procedure application, and 
        the remaining sections cover sequencing, conditional evaluation, recursion, mapping, continuations, 
        delayed evaluation, multiple values, and evaluation of programs constructed at run time.

    Section 5.1. Procedure Application

        syntax: (expr0 expr1 ...) 
        returns: values of applying the value of expr0 to the values of expr1 ...

        Procedure application is the most basic Scheme control structure. 
        Any structured form without a syntax keyword in the first position is a procedure application. 
        The expressions expr0 and expr1 ... are evaluated; each should evaluate to a single value. 
        After each of these expressions has been evaluated, the value of expr0 is applied to the values of expr1 .... 
        If expr0 does not evaluate to a procedure, or if the procedure does not accept the number of arguments provided, 
        an exception with condition type &assertion is raised.

        The order in which the procedure and argument expressions are evaluated is unspecified. It may be left to right, right to left, or any other order. 
        The evaluation is guaranteed to be sequential, however: whatever order is chosen, each expression is fully evaluated before evaluation of the next is started.

            (+ 3 4) <graphic> 7 

            ((if (odd? 3) + -) 6 2) <graphic> 8 

            ((lambda (x) x) 5) <graphic> 5 

            (let ([f (lambda (x) (+ x x))])
              (f 8)) <graphic> 16

        procedure: (apply procedure obj ... list) 
        returns: the values of applying procedure to obj ... and the elements of list 
        libraries: (rnrs base), (rnrs)

        apply invokes procedure, passing the first obj as the first argument, the second obj as the second argument, and so on for each object in obj ..., and 
        passing the elements of list in order as the remaining arguments. Thus, procedure is called with as many arguments as there are objs plus elements of list.

        apply is useful when some or all of the arguments to be passed to a procedure are in a list, since it frees the programmer from explicitly destructuring the list.

            (apply + '(4 5)) <graphic> 9 

            (apply min '(6 8 3 2 5)) <graphic> 2 

            (apply min  5 1 3 '(6 8 3 2 5)) <graphic> 1 

            (apply vector 'a 'b '(c d e)) <graphic> #(a b c d e) 

            (define first
              (lambda (ls)
                (apply (lambda (x . y) x) ls)))
            (define rest
              (lambda (ls)
                (apply (lambda (x . y) y) ls)))
            (first '(a b c d)) <graphic> a
            (rest '(a b c d)) <graphic> (b c d) 

            (apply append
              '(1 2 3)
              '((a b) (c d e) (f))) <graphic> (1 2 3 a b c d e f)

    Section 5.2. Sequencing

        syntax: (begin expr1 expr2 ...) 
        returns: the values of the last subexpression 
        libraries: (rnrs base), (rnrs)

        The expressions expr1 expr2 ... are evaluated in sequence from left to right. begin is used to sequence assignments, input/output, or other operations that cause side effects.

            (define x 3)
            (begin
              (set! x (+ x 1))
              (+ x x)) <graphic> 8

        A begin form may contain zero or more definitions in place of the expressions expr1 expr2 ..., in which case it is considered to be a definition and may appear only where definitions are valid.

            (let ()
              (begin (define x 3) (define y 4))
              (+ x y)) <graphic> 7

        This form of begin is primarily used by syntactic extensions that must expand into multiple definitions. (See page 101.)

        The bodies of many syntactic forms, including lambda, case-lambda, let, let*, letrec, and letrec*, as well as the result clauses of cond, case, and do, 
        are treated as if they were inside an implicit begin; i.e., the expressions making up the body or result clause are executed in sequence, with the values of the last expression being returned.

            (define swap-pair!
              (lambda (x)
                (let ([temp (car x)])
                  (set-car! x (cdr x))
                  (set-cdr! x temp)
                  x)))

            (swap-pair! (cons 'a 'b)) <graphic> (b . a)

    Section 5.3. Conditionals

        syntax: (if test consequent alternative) 
        syntax: (if test consequent) 
        returns: the values of consequent or alternative depending on the value of test 
        libraries: (rnrs base), (rnrs)

        The test, consequent, and alternative subforms must be expressions. 
        If test evaluates to a true value (anything other than #f), consequent is evaluated and its values are returned. 
        Otherwise, alternative is evaluated and its values are returned. 
        With the second, "one-armed," form, which has no alternative, the result is unspecified if test evaluates to false.

            (let ([ls '(a b c)])
              (if (null? ls)
                  '()
                  (cdr ls))) <graphic> (b c) 

            (let ([ls '()])
              (if (null? ls)
                  '()
                  (cdr ls))) <graphic> () 

            (let ([abs
                   (lambda (x)
                     (if (< x 0)
                         (- 0 x)
                         x))])
              (abs -4)) <graphic> 4 

            (let ([x -4])
              (if (< x 0)
                  (list 'minus (- 0 x))
                  (list 'plus 4))) <graphic> (minus 4)

        procedure: (not obj) 
        returns: #t if obj is false, #f otherwise 
        libraries: (rnrs base), (rnrs)

        not is equivalent to (lambda (x) (if x #f #t)).

            (not #f) <graphic> #t
            (not #t) <graphic> #f
            (not '()) <graphic> #f
            (not (< 4 5)) <graphic> #f

        syntax: (and expr ...) 
        returns: see below 
        libraries: (rnrs base), (rnrs)

        If no subexpressions are present, the and form evaluates to #t. 
        Otherwise, and evaluates each subexpression in sequence from left to right until only one subexpression remains or a subexpression returns #f. 
        If one subexpression remains, it is evaluated and its values are returned. 
        If a subexpression returns #f, and returns #f without evaluating the remaining subexpressions. 
        A syntax definition of and appears on page 62.

            (define-syntax and
              (syntax-rules ()
                [(_) #t]
                [(_ e) e]
                [(_ e1 e2 e3 ...)
                 (if e1 (and e2 e3 ...) #f)]))

            (let ([x 3])
              (and (> x 2) (< x 4))) <graphic> #t 

            (let ([x 5])
              (and (> x 2) (< x 4))) <graphic> #f 

            (and #f '(a b) '(c d)) <graphic> #f
            (and '(a b) '(c d) '(e f)) <graphic> (e f)

        syntax: (or expr ...) 
        returns: see below 
        libraries: (rnrs base), (rnrs)

        If no subexpressions are present, the or form evaluates to #f. 
        Otherwise, or evaluates each subexpression in sequence from left to right until only one subexpression remains or a subexpression returns a value other than #f. 
        If one subexpression remains, it is evaluated and its values are returned. 
        If a subexpression returns a value other than #f, or returns that value without evaluating the remaining subexpressions. A syntax definition of or appears on page 63.

            (define-syntax or
              (syntax-rules ()
                [(_) #f]
                [(_ e) e]
                [(_ e1 e2 e3 ...)
                 (let ([t e1])
                   (if t t (or e2 e3 ...)))]))

            (define-syntax or ; incorrect!
              (syntax-rules ()
                [(_) #f]
                [(_ e1 e2 ...)
                  (let ([t e1])
                    (if t t (or e2 ...)))]))

            (let ([x 3])
              (or (< x 2) (> x 4))) <graphic> #f 

            (let ([x 5])
              (or (< x 2) (> x 4))) <graphic> #t 

            (or #f '(a b) '(c d)) <graphic> (a b)

        syntax: (cond clause1 clause2 ...) 
        returns: see below 
        libraries: (rnrs base), (rnrs)

        Each clause but the last must take one of the forms below.

            (test)
            (test expr1 expr2 ...)
            (test => expr)

        The last clause may be in any of the above forms, or it may be an "else clause" of the form

            (else expr1 expr2 ...)

        Each test is evaluated in order until one evaluates to a true value or until all of the tests have been evaluated. 
        If the first clause whose test evaluates to a true value is in the first form given above, the value of test is returned.

        If the first clause whose test evaluates to a true value is in the second form given above, the expressions expr1 expr2... are evaluated in sequence and the values of the last expression are returned.

        If the first clause whose test evaluates to a true value is in the third form given above, the expression expr is evaluated. 
        The value should be a procedure of one argument, which is applied to the value of test. The values of this application are returned.

        If none of the tests evaluates to a true value and an else clause is present, the expressions expr1 expr2 ... of the else clause are evaluated in sequence and the values of the last expression are returned.

        If none of the tests evaluates to a true value and no else clause is present, the value or values are unspecified.

        See page 305 for a syntax definition of cond.

            (define-syntax cond
              (lambda (x)
                (syntax-case x ()
                  [(_ c1 c2 ...)
                   (let f ([c1 #'c1] [cmore #'(c2 ...)])
                     (if (null? cmore)
                         (syntax-case c1 (else =>)
                           [(else e1 e2 ...) #'(begin e1 e2 ...)]
                           [(e0) #'(let ([t e0]) (if t t))]
                           [(e0 => e1) #'(let ([t e0]) (if t (e1 t)))]
                           [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...))])
                         (with-syntax ([rest (f (car cmore) (cdr cmore))])
                           (syntax-case c1 (=>)
                             [(e0) #'(let ([t e0]) (if t t rest))]
                             [(e0 => e1) #'(let ([t e0]) (if t (e1 t) rest))]
                             [(e0 e1 e2 ...)
                              #'(if e0 (begin e1 e2 ...) rest)]))))])))

            (let ([x 0])
              (cond
                [(< x 0) (list 'minus (abs x))]
                [(> x 0) (list 'plus x)]
                [else (list 'zero x)])) <graphic> (zero 0) 

            (define select
              (lambda (x)
                (cond
                  [(not (symbol? x))]
                  [(assq x '((a . 1) (b . 2) (c . 3))) => cdr]
                  [else 0])))

            (select 3) <graphic> #t
            (select 'b) <graphic> 2
            (select 'e) <graphic> 0






    Section 5.4. Recursion and Iteration



    Section 5.5. Mapping and Folding



    Section 5.6. Continuations



    Section 5.7. Delayed Evaluation



    Section 5.8. Multiple Values



    Section 5.9. Eval


