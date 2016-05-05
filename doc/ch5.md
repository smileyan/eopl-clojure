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

        syntax: else 
        syntax: => 
        libraries: (rnrs base), (rnrs exceptions), (rnrs)

        These identifiers are auxiliary keywords for cond. Both also serve as auxiliary keywords for guard, and else also serves as an auxiliary keyword for case. 
        It is a syntax violation to reference these identifiers except in contexts where they are recognized as auxiliary keywords.

        syntax: (when test-expr expr1 expr2 ...) 
        syntax: (unless test-expr expr1 expr2 ...) 
        returns: see below 
        libraries: (rnrs control), (rnrs)

        For when, if test-expr evaluates to a true value, the expressions expr1 expr2 ... are evaluated in sequence, and the values of the last expression are returned. 
        If test-expr evaluates to false, none of the other expressions are evaluated, and the value or values of when are unspecified.

        For unless, if test-expr evaluates to false, the expressions expr1 expr2 ... are evaluated in sequence, and the values of the last expression are returned. 
        If test-expr evaluates to a true value, none of the other expressions are evaluated, and the value or values of unless are unspecified.

        A when or unless expression is usually clearer than the corresponding "one-armed" if expression.


            (let ([x -4] [sign 'plus])
              (when (< x 0)
                (set! x (- 0 x))
                (set! sign 'minus))
              (list sign x)) <graphic> (minus 4) 

            (define check-pair
              (lambda (x)
                (unless (pair? x)
                  (syntax-violation 'check-pair "invalid argument" x))
                x)) 

            (check-pair '(a b c)) <graphic> (a b c)

        when may be defined as follows:

            (define-syntax when
              (syntax-rules ()
                [(_ e0 e1 e2 ...)
                 (if e0 (begin e1 e2 ...))]))

        unless may be defined as follows:

            (define-syntax unless
              (syntax-rules ()
                [(_ e0 e1 e2 ...)
                 (if (not e0) (begin e1 e2 ...))]))

        or in terms of when as follows:

            (define-syntax unless
              (syntax-rules ()
                [(_ e0 e1 e2 ...)
                 (when (not e0) e1 e2 ...)]))

        syntax: (case expr0 clause1 clause2 ...) 
        returns: see below 
        libraries: (rnrs base), (rnrs)

        Each clause but the last must take the form

            ((key ...) expr1 expr2 ...)

        where each key is a datum distinct from the other keys. The last clause may be in the above form or it may be an else clause of the form

            (else expr1 expr2 ...)

        expr0 is evaluated and the result is compared (using eqv?) against the keys of each clause in order. 
        If a clause containing a matching key is found, the expressions expr1 expr2 ... are evaluated in sequence and the values of the last expression are returned.

        If none of the clauses contains a matching key and an else clause is present, the expressions expr1 expr2 ... of 
        the else clause are evaluated in sequence and the values of the last expression are returned.

        If none of the clauses contains a matching key and no else clause is present, the value or values are unspecified.

            (define-syntax case
              (lambda (x)
                (syntax-case x ()
                  [(_ e c1 c2 ...)
                   #`(let ([t e])
                       #,(let f ([c1 #'c1] [cmore #'(c2 ...)])
                           (if (null? cmore)
                               (syntax-case c1 (else)
                                 [(else e1 e2 ...) #'(begin e1 e2 ...)]
                                 [((k ...) e1 e2 ...)
                                  #'(if (memv t '(k ...)) (begin e1 e2 ...))])
                               (syntax-case c1 ()
                                 [((k ...) e1 e2 ...)
                                  #`(if (memv t '(k ...))
                                        (begin e1 e2 ...)
                                        #,(f (car cmore) (cdr cmore)))]))))])))

        See page 306 for a syntax definition of case.

            (let ([x 4] [y 5])
              (case (+ x y)
                [(1 3 5 7 9) 'odd]
                [(0 2 4 6 8) 'even]
                [else 'out-of-range])) <graphic> odd



    Section 5.4. Recursion and Iteration

        syntax: (let name ((var expr) ...) body1 body2 ...) 
        returns: values of the final body expression 
        libraries: (rnrs base), (rnrs)

        This form of let, called named let, is a general-purpose iteration and recursion construct. 
        It is similar to the more common form of let (see Section 4.4) in the binding of the variables var ... 
        to the values of expr ... within the body body1 body2 ..., which is processed and evaluated like a lambda body. 
        In addition, the variable name is bound within the body to a procedure that 
        may be called to recur or iterate; 
        the arguments to the procedure become the new values of the variables var ....

        A named let expression of the form

            (let name ((var expr) ...)
              body1 body2 ...)

        can be rewritten with letrec as follows.

            ((letrec ((name (lambda (var ...) body1 body2 ...)))
               name)
             expr ...)

        A syntax definition of let that implements this transformation and handles unnamed let as well can be found on page 312.

            (define-syntax let
              (lambda (x)
                (syntax-case x ()
                  [(_ ((x e) ...) b1 b2 ...)
                   #'((lambda (x ...) b1 b2 ...) e ...)]
                  [(_ f ((x e) ...) b1 b2 ...)
                   (identifier? #'f)
                   #'((rec f (lambda (x ...) b1 b2 ...)) e ...)])))

        The procedure divisors defined below uses named let to compute the nontrivial divisors of a nonnegative integer.

            (define divisors
              (lambda (n)
                (let f ([i 2])
                  (cond
                    [(>= i n) '()]
                    [(integer? (/ n i)) (cons i (f (+ i 1)))]
                    [else (f (+ i 1))])))) 

            (divisors 5) <graphic> ()
            (divisors 32) <graphic> (2 4 8 16)

        The version above is non-tail-recursive when a divisor is found and tail-recursive when a divisor is not found. 
        The version below is fully tail-recursive. 
        It builds up the list in reverse order, 
        but this is easy to remedy, if desired, by reversing the list on exit.

            (define divisors
              (lambda (n)
                (let f ([i 2] [ls '()])
                  (cond
                    [(>= i n) ls]
                    [(integer? (/ n i)) (f (+ i 1) (cons i ls))]
                    [else (f (+ i 1) ls)]))))

        syntax: (do ((var init update) ...) (test result ...) expr ...) 
        returns: the values of the last result expression 
        libraries: (rnrs control), (rnrs)

        do allows a common restricted form of iteration to be expressed succinctly. 
        The variables var ... are bound initially to the values of init ... and 
                              are rebound on each subsequent iteration to the values of update .... 
        The expressions test, update ..., expr ..., and result ... are all within the scope of the bindings established for var ....

        On each step, the test expression test is evaluated. 
        If the value of test is true, iteration ceases, the expressions result ... are evaluated in sequence, 
        and the values of the last expression are returned. 
        If no result expressions are present, the value or values of the do expression are unspecified.

        If the value of test is false, the expressions expr ... are evaluated in sequence, 
        the expressions update ... are evaluated, new bindings for var ... to the values of update ... are created, and iteration continues.

        The expressions expr ... are evaluated only for effect and are often omitted entirely. 
        Any update expression may be omitted, in which case the effect is the same as if the update were simply the corresponding var.

        Although looping constructs in most languages require that the loop iterands be updated via assignment, 
        do requires the loop iterands var ... to be updated via rebinding. In fact, no side effects are involved in the evaluation of a do expression unless they are performed explicitly by its subexpressions.

        See page 313 for a syntax definition of do.

        The definitions of factorial and fibonacci below are straightforward translations of the tail-recursive named-let versions given in Section 3.2.

            (define factorial
              (lambda (n)
                (do ([i n (- i 1)] [a 1 (* a i)])
                    ((zero? i) a)))) 

            (factorial 10) <graphic> 3628800 

            (define fibonacci
              (lambda (n)
                (if (= n 0)
                    0
                    (do ([i n (- i 1)] [a1 1 (+ a1 a2)] [a2 0 a1])
                        ((= i 1) a1))))) 

            (fibonacci 6) <graphic> 8

        The definition of divisors below is similar to the tail-recursive definition of divisors given with the description of named let above.

            (define divisors
              (lambda (n)
                (do ([i 2 (+ i 1)]
                     [ls '()
                         (if (integer? (/ n i))
                             (cons i ls)
                             ls)])
                    ((>= i n) ls))))

        The definition of scale-vector! below, which scales each element of a vector v by a constant k, demonstrates a nonempty do body.

            (define scale-vector!
              (lambda (v k)
                (let ([n (vector-length v)])
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                    (vector-set! v i (* (vector-ref v i) k)))))) 

            (define vec (vector 1 2 3 4 5))
            (scale-vector! vec 2)
            vec <graphic> #(2 4 6 8 10)

    Section 5.5. Mapping and Folding

        When a program must recur or iterate over the elements of a list, a mapping or folding operator is often more convenient. 
        These operators abstract away from null checks and explicit recursion by applying a procedure to the elements of the list one by one. A few mapping operators are also available for vectors and strings.

        procedure: (map procedure list1 list2 ...) 
        returns: list of results 
        libraries: (rnrs base), (rnrs)

        map applies procedure to corresponding elements of the lists list1 list2 ... and returns a list of the resulting values. 
        The lists list1 list2 ... must be of the same length. procedure should accept as many arguments as there are lists, 
        should return a single value, and should not mutate the list arguments.

            (map abs '(1 -2 3 -4 5 -6)) <graphic> (1 2 3 4 5 6) 

            (map (lambda (x y) (* x y))
                 '(1 2 3 4)
                 '(8 7 6 5)) <graphic> (8 14 18 20)

        While the order in which the applications themselves occur is not specified, the order of the values in the output list is the same as that of the corresponding values in the input lists.

        map might be defined as follows.

            (define map
              (lambda (f ls . more)
                (if (null? more)
                    (let map1 ([ls ls])
                      (if (null? ls)
                          '()
                          (cons (f (car ls))
                                (map1 (cdr ls)))))
                    (let map-more ([ls ls] [more more])
                      (if (null? ls)
                          '()
                          (cons
                            (apply f (car ls) (map car more))
                            (map-more (cdr ls) (map cdr more))))))))

        No error checking is done by this version of map; f is assumed to be a procedure and the other arguments are assumed to be proper lists of the same length. 
        An interesting feature of this definition is that map uses itself to pull out the cars and cdrs of the list of input lists; this works because of the special treatment of the single-list case.

        procedure: (for-each procedure list1 list2 ...) 
        returns: unspecified 
        libraries: (rnrs base), (rnrs)

        for-each is similar to map except that for-each does not create and return a list of the resulting values, 
        and for-each guarantees to perform the applications in sequence over the elements from left to right. 
        procedure should accept as many arguments as there are lists and should not mutate the list arguments. 
        for-each may be defined without error checks as follows.

            (define for-each
              (lambda (f ls . more)
                (do ([ls ls (cdr ls)] [more more (map cdr more)])
                    ((null? ls))
                  (apply f (car ls) (map car more))))) 

            (let ([same-count 0])
              (for-each
                (lambda (x y)
                  (when (= x y)
                    (set! same-count (+ same-count 1))))
                '(1 2 3 4 5 6)
                '(2 3 3 4 7 6))
              same-count) <graphic> 3

        procedure: (exists procedure list1 list2 ...) 
        returns: see below 
        libraries: (rnrs lists), (rnrs)

        The lists list1 list2 ... must be of the same length. 
        procedure should accept as many arguments as there are lists and should not mutate the list arguments. 
        If the lists are empty, exists returns #f. 
        Otherwise, exists applies procedure to corresponding elements of the lists list1 list2 ... in sequence until 
        either the lists each have only one element or procedure returns a true value t. 
        In the former case, exists tail-calls procedure, applying it to the remaining element of each list. In the latter case, exists returns t.

            (exists symbol? '(1.0 #\a "hi" '())) <graphic> #f 

            (exists member
                    '(a b c)
                    '((c b) (b a) (a c))) <graphic> (b a) 

            (exists (lambda (x y z) (= (+ x y) z))
                    '(1 2 3 4)
                    '(1.2 2.3 3.4 4.5)
                    '(2.3 4.4 6.4 8.6)) <graphic> #t

        exists may be defined (somewhat inefficiently and without error checks) as follows:

            (define exists
              (lambda (f ls . more)
                (and (not (null? ls))
                  (let exists ([x (car ls)] [ls (cdr ls)] [more more])
                    (if (null? ls)
                        (apply f x (map car more))
                        (or (apply f x (map car more))
                            (exists (car ls) (cdr ls) (map cdr more))))))))

        procedure: (for-all procedure list1 list2 ...) 
        returns: see below 
        libraries: (rnrs lists), (rnrs)

        The lists list1 list2 ... must be of the same length. 
        procedure should accept as many arguments as there are lists and should not mutate the list arguments. 
        If the lists are empty, for-all returns #t. 
        Otherwise, for-all applies procedure to corresponding elements of the lists list1 list2 ... in sequence 
        until either the lists each have only one element left or procedure returns #f. 
        In the former case, for-all tail-calls procedure, applying it to the remaining element of each list. In the latter case, for-all returns #f.

            (for-all symbol? '(a b c d)) <graphic> #t 

            (for-all =
                     '(1 2 3 4)
                     '(1.0 2.0 3.0 4.0)) <graphic> #t 

            (for-all (lambda (x y z) (= (+ x y) z))
                     '(1 2 3 4)
                     '(1.2 2.3 3.4 4.5)
                     '(2.2 4.3 6.5 8.5)) <graphic> #f

        for-all may be defined (somewhat inefficiently and without error checks) as follows:

            (define for-all
              (lambda (f ls . more)
                (or (null? ls)
                  (let for-all ([x (car ls)] [ls (cdr ls)] [more more])
                    (if (null? ls)
                        (apply f x (map car more))
                        (and (apply f x (map car more))
                             (for-all (car ls) (cdr ls) (map cdr more))))))))

        procedure: (fold-left procedure obj list1 list2 ...) 
        returns: see below 
        libraries: (rnrs lists), (rnrs)

        The list arguments should all have the same length. 
        procedure should accept one more argument than the number of list arguments and return a single value. 
        It should not mutate the list arguments.

        fold-left returns obj if the list arguments are empty. 
        If they are not empty, fold-left applies procedure to obj and the cars of list1 list2 ..., 
        then recurs with the value returned by procedure in place of obj and the cdr of each list in place of the list.

            (fold-left cons '() '(1 2 3 4)) <graphic> ((((() . 1) . 2) . 3) . 4) 

            (fold-left
              (lambda (a x) (+ a (* x x)))
              0 '(1 2 3 4 5)) <graphic> 55 

            (fold-left
              (lambda (a . args) (append args a))
              '(question)
              '(that not to)
              '(is to be)
              '(the be: or)) <graphic> (to be or not to be: that is the question)

        procedure: (fold-right procedure obj list1 list2 ...) 
        returns: see below 
        libraries: (rnrs lists), (rnrs)

        The list arguments should all have the same length. 
        procedure should accept one more argument than the number of list arguments and return a single value. 
        It should not mutate the list arguments.

        fold-right returns obj if the list arguments are empty. 
        If they are not empty, fold-right recurs with the cdr of each list replacing the list, 
        then applies procedure to the cars of list1 list2 ... and the result returned by the recursion.

            (fold-right cons '() '(1 2 3 4)) <graphic> (1 2 3 4) 

            (fold-right
              (lambda (x a) (+ a (* x x)))
              0 '(1 2 3 4 5)) <graphic> 55 

            (fold-right
              (lambda (x y a) (cons* x y a))   => (parting is such sweet sorrow
              '((with apologies))                  gotta go see ya tomorrow
              '(parting such sorrow go ya)         (with apologies))
              '(is sweet gotta see tomorrow))

        procedure: (vector-map procedure vector1 vector1 ...) 
        returns: vector of results 
        libraries: (rnrs base), (rnrs)

        vector-map applies procedure to corresponding elements of vector1 vector2 ... and returns a vector of the resulting values. 
        The vectors vector1 vector2 ... must be of the same length, and procedure should accept as many arguments as there are vectors and 
        return a single value.

            (vector-map abs '#(1 -2 3 -4 5 -6)) <graphic> #(1 2 3 4 5 6)

            (vector-map (lambda (x y) (* x y))
              '#(1 2 3 4)
              '#(8 7 6 5)) <graphic> #(8 14 18 20)

        While the order in which the applications themselves occur is not specified, 
        the order of the values in the output vector is the same as that of the corresponding values in the input vectors.

        procedure: (vector-for-each procedure vector1 vector2 ...) 
        returns: unspecified 
        libraries: (rnrs base), (rnrs)

        vector-for-each is similar to vector-map except that vector-for-each does not create and return a vector of the resulting values, 
        and vector-for-each guarantees to perform the applications in sequence over the elements from left to right.

            (let ([same-count 0])
              (vector-for-each
                (lambda (x y)
                  (when (= x y)
                    (set! same-count (+ same-count 1))))
                '#(1 2 3 4 5 6)
                '#(2 3 3 4 7 6))
              same-count) <graphic> 3

        procedure: (string-for-each procedure string1 string2 ...) 
        returns: unspecified 
        libraries: (rnrs base), (rnrs)

        string-for-each is similar to for-each and vector-for-each except that the inputs are strings rather than lists or vectors.

            (let ([ls '()])
              (string-for-each
                (lambda r (set! ls (cons r ls)))
                "abcd"
                "===="
                "1234")
              (map list->string (reverse ls))) <graphic> ("a=1" "b=2" "c=3" "d=4")

    Section 5.6. Continuations

      Continuations in Scheme are procedures that represent the remainder of a computation from a given point in the computation. They may be obtained with call-with-current-continuation, which can be abbreviated to call/cc.

      procedure: (call/cc procedure) 
      procedure: (call-with-current-continuation procedure) 
      returns: see below 
      libraries: (rnrs base), (rnrs)

      These procedures are the same. The shorter name is often used for the obvious reason that it requires fewer keystrokes to type.

      call/cc obtains its continuation and passes it to procedure, which should accept one argument. 
      The continuation itself is represented by a procedure. Each time this procedure is applied to zero or more values, 
      it returns the values to the continuation of the call/cc application. That is, when the continuation procedure is called, it returns its arguments as the values of the application of call/cc.

      If procedure returns normally when passed the continuation procedure, the values returned by call/cc are the values returned by procedure.

      Continuations allow the implementation of nonlocal exits, backtracking [14,29], coroutines [16], and multitasking [10,32].

      The example below illustrates the use of a continuation to perform a nonlocal exit from a loop.

        (define member
          (lambda (x ls)
            (call/cc
              (lambda (break)
                (do ([ls ls (cdr ls)])
                    ((null? ls) #f)
                  (when (equal? x (car ls))
                    (break ls))))))) 

        (member 'd '(a b c)) <graphic> #f
        (member 'b '(a b c)) <graphic> (b c)

      Additional examples are given in Sections 3.3 and 12.11.

      Section 3.3. Continuations
        During the evaluation of a Scheme expression, the implementation must keep track of two things: 
        (1) what to evaluate and (2) what to do with the value. Consider the evaluation of (null? x) within the expression below.

          (if (null? x) (quote ()) (cdr x))

        The implementation must first evaluate (null? x) and, based on its value, evaluate either (quote ()) or (cdr x). 
        "What to evaluate" is (null? x), and "what to do with the value" is to make the decision which of (quote ()) and (cdr x) to evaluate and to do so. 
        We call "what to do with the value" the continuation of a computation.

        Thus, at any point during the evaluation of any expression, there is a continuation ready to complete, or at least continue, the computation from that point. 
        Let's assume that x has the value (a b c). We can isolate six continuations during the evaluation of (if (null? x) (quote ()) (cdr x)), the continuations waiting for

          the value of (if (null? x) (quote ()) (cdr x)),
          the value of (null? x),
          the value of null?,
          the value of x,
          the value of cdr, and
          the value of x (again).

        The continuation of (cdr x) is not listed because it is the same as the one waiting for (if (null? x) (quote ()) (cdr x)).

        Scheme allows the continuation of any expression to be captured with the procedure call/cc. 
        call/cc must be passed a procedure p of one argument. call/cc constructs a concrete representation of the current continuation and passes it to p. 
        The continuation itself is represented by a procedure k. Each time k is applied to a value, it returns the value to the continuation of the call/cc application. 
        This value becomes, in essence, the value of the application of call/cc.

        If p returns without invoking k, the value returned by the procedure becomes the value of the application of call/cc.

        Consider the simple examples below.

          (call/cc
            (lambda (k)
              (* 5 4))) <graphic> 20 

          (call/cc
            (lambda (k)
              (* 5 (k 4)))) <graphic> 4 

          (+ 2
             (call/cc
               (lambda (k)
                 (* 5 (k 4))))) <graphic> 6

        In the first example, the continuation is captured and bound to k, but k is never used, so the value is simply the product of 5 and 4. 
        In the second, the continuation is invoked before the multiplication, so the value is the value passed to the continuation, 4. 
        In the third, the continuation includes the addition by 2; thus, the value is the value passed to the continuation, 4, plus 2.

        Here is a less trivial example, showing the use of call/cc to provide a nonlocal exit from a recursion.

          (define product
            (lambda (ls)
              (call/cc
                (lambda (break)
                  (let f ([ls ls])
                    (cond
                      [(null? ls) 1]
                      [(= (car ls) 0) (break 0)]
                      [else (* (car ls) (f (cdr ls)))]))))))

          (product '(1 2 3 4 5)) <graphic> 120
          (product '(7 3 8 0 1 9 5)) <graphic> 0

        The nonlocal exit allows product to return immediately, without performing the pending multiplications, when a zero value is detected.

        Each of the continuation invocations above returns to the continuation while control remains within the procedure passed to call/cc. 
        The following example uses the continuation after this procedure has already returned.

          (let ([x (call/cc (lambda (k) k))])
            (x (lambda (ignore) "hi"))) <graphic> "hi"

        The continuation captured by this invocation of call/cc may be described as "Take the value, bind it to x, and apply the value of x to the value of (lambda (ignore) "hi")." 
        Since (lambda (k) k) returns its argument, x is bound to the continuation itself; this continuation is applied to the procedure resulting from the evaluation of (lambda (ignore) "hi"). 
        This has the effect of binding x (again!) to this procedure and applying the procedure to itself. The procedure ignores its argument and returns "hi".

        The following variation of the example above is probably the most confusing Scheme program of its size; it might be easy to guess what it returns, but it takes some thought to figure out why.

        (((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!") <graphic> "HEY!"

        The value of the call/cc is its own continuation, as in the preceding example. 
        This is applied to the identity procedure (lambda (x) x), so the call/cc returns a second time with this value. 
        Then, the identity procedure is applied to itself, yielding the identity procedure. 
        This is finally applied to "HEY!", yielding "HEY!".

        Continuations used in this manner are not always so puzzling. 
        Consider the following definition of factorial that saves the continuation at the base of the recursion before returning 1, by assigning the top-level variable retry.

          (define retry #f) 

          (define factorial
            (lambda (x)
              (if (= x 0)
                  (call/cc (lambda (k) (set! retry k) 1))
                  (* x (factorial (- x 1))))))

        With this definition, factorial works as we expect factorial to work, except it has the side effect of assigning retry.

          (factorial 4) <graphic> 24
          (retry 1) <graphic> 24
          (retry 2) <graphic> 48

        The continuation bound to retry might be described as "Multiply the value by 1, then multiply this result by 2, then multiply this result by 3, then multiply this result by 4." 
        If we pass the continuation a different value, i.e., not 1, we will cause the base value to be something other than 1 and hence change the end result.

          (retry 2) <graphic> 48
          (retry 5) <graphic> 120

        This mechanism could be the basis for a breakpoint package implemented with call/cc; each time a breakpoint is encountered, 
        the continuation of the breakpoint is saved so that the computation may be restarted from the breakpoint (more than once, if desired).

        Continuations may be used to implement various forms of multitasking. The simple "light-weight process" mechanism defined below allows multiple computations to be interleaved. 
        Since it is nonpreemptive, it requires that each process voluntarily "pause" from time to time in order to allow the others to run.

          (define lwp-list '())
          (define lwp
            (lambda (thunk)
              (set! lwp-list (append lwp-list (list thunk))))) 

          (define start
            (lambda ()
              (let ([p (car lwp-list)])
                (set! lwp-list (cdr lwp-list))
                (p))))

          (define pause
            (lambda ()
              (call/cc
                (lambda (k)
                  (lwp (lambda () (k #f)))
                  (start)))))

    Section 5.7. Delayed Evaluation

      The syntactic form delay and the procedure force may be used in combination to implement lazy evaluation. 
      An expression subject to lazy evaluation is not evaluated until its value is required 
      and, once evaluated, is never reevaluated.

      syntax: (delay expr) 
      returns: a promise 
      procedure: (force promise) 
      returns: result of forcing promise 
      libraries: (rnrs r5rs)

      The first time a promise created by delay is forced (with force), it evaluates expr, "remembering" the resulting value. 
      Thereafter, each time the promise is forced, it returns the remembered value instead of reevaluating expr.

      delay and force are typically used only in the absence of side effects, e.g., assignments, so that the order of evaluation is unimportant.

      The benefit of using delay and force is that some amount of computation might be avoided altogether if it is delayed until absolutely required. 
      Delayed evaluation may be used to construct conceptually infinite lists, or streams. 
      The example below shows how a stream abstraction may be built with delay and force. 
      A stream is a promise that, when forced, returns a pair whose cdr is a stream.

        (define stream-car
          (lambda (s)
            (car (force s)))) 

        (define stream-cdr
          (lambda (s)
            (cdr (force s)))) 

        (define counters
          (let next ([n 1])
            (delay (cons n (next (+ n 1)))))) 

        (stream-car counters) <graphic> 1 

        (stream-car (stream-cdr counters)) <graphic> 2 

        (define stream-add
          (lambda (s1 s2)
            (delay (cons
                     (+ (stream-car s1) (stream-car s2))
                     (stream-add (stream-cdr s1) (stream-cdr s2)))))) 

        (define even-counters
          (stream-add counters counters)) 

        (stream-car even-counters) <graphic> 2 

        (stream-car (stream-cdr even-counters)) <graphic> 4

        delay may be defined by

          (define-syntax delay
            (syntax-rules ()
              [(_ expr) (make-promise (lambda () expr))]))

        where make-promise might be defined as follows.

        (define make-promise
          (lambda (p)
            (let ([val #f] [set? #f])
              (lambda ()
                (unless set?
                  (let ([x (p)])
                    (unless set?
                      (set! val x)
                      (set! set? #t))))
                val))))

        With this definition of delay, force simply invokes the promise to force evaluation or to retrieve the saved value.

        (define force
          (lambda (promise)
            (promise)))

        The second test of the variable set? in make-promise is necessary in the event that, 
        as a result of applying p, the promise is recursively forced. 
        Since a promise must always return the same value, the result of the first application of p to complete is returned.

        Whether delay and force handle multiple return values is unspecified; 
        the implementation given above does not, but the following version does, with the help of call-with-values and apply.

        (define make-promise
          (lambda (p)
            (let ([vals #f] [set? #f])
              (lambda ()
                (unless set?
                  (call-with-values p
                    (lambda x
                      (unless set?
                        (set! vals x)
                        (set! set? #t)))))
                (apply values vals)))))
        (define p (delay (values 1 2 3)))
        (force p) <graphic> 1
                            2
                            3
        (call-with-values (lambda () (force p)) +) <graphic> 6

        Neither implementation is quite right, since force must raise an exception with condition type &assertion if its argument is not a promise. 
        Since distinguishing procedures created by make-promise from other procedures is impossible, force cannot do so reliably. 
        The following reimplementation of make-promise and force represents promises as records of the type promise to allow force to make the required check.

        (define-record-type promise
          (fields (immutable p) (mutable vals) (mutable set?))
          (protocol (lambda (new) (lambda (p) (new p #f #f))))) 

        (define force
          (lambda (promise)
            (unless (promise? promise)
              (assertion-violation 'promise "invalid argument" promise))
            (unless (promise-set? promise)
              (call-with-values (promise-p promise)
                (lambda x
                  (unless (promise-set? promise)
                    (promise-vals-set! promise x)
                    (promise-set?-set! promise #t)))))
            (apply values (promise-vals promise))))

    Section 5.8. Multiple Values

      While all Scheme primitives and most user-defined procedures return exactly one value, 
      some programming problems are best solved by returning zero values, more than one value, or even a variable number of values. 
      For example, a procedure that partitions a list of values into two sublists needs to return two values. 
      While it is possible for the producer of multiple values to package them into a data structure and for the consumer to extract them, 
      it is often cleaner to use the built-in multiple-values interface. 
      This interface consists of two procedures: values and call-with-values. 
      The former produces multiple values and the latter links procedures that produce multiple-value values with procedures that consume them.

      procedure: (values obj ...) 
      returns: obj ... 
      libraries: (rnrs base), (rnrs)

      The procedure values accepts any number of arguments and simply passes (returns) the arguments to its continuation.

      (values) <graphic>

      (values 1) <graphic> 1 

      (values 1 2 3) <graphic> 1
                      2
                      3 

      (define head&tail
        (lambda (ls)
          (values (car ls) (cdr ls)))) 

      (head&tail '(a b c)) <graphic> a
                            (b c)

      procedure: (call-with-values producer consumer) 
      returns: see below 
      libraries: (rnrs base), (rnrs)

      producer and consumer must be procedures. call-with-values applies consumer to the values returned by invoking producer without arguments.

      (call-with-values
        (lambda () (values 'bond 'james))
        (lambda (x y) (cons y x))) <graphic> (james . bond) 

      (call-with-values values list) <graphic> '()

      In the second example, values itself serves as the producer. It receives no arguments and thus returns no values. list is thus applied to no arguments and so returns the empty list.

      The procedure dxdy defined below computes the change in x and y coordinates for a pair of points whose coordinates are represented by (x . y) pairs.

      (define dxdy
        (lambda (p1 p2)
          (values (- (car p2) (car p1))
                  (- (cdr p2) (cdr p1))))) 

      (dxdy '(0 . 0) '(0 . 5)) <graphic> 0
                                         5

      dxdy can be used to compute the length and slope of a segment represented by two endpoints.

      (define segment-length
        (lambda (p1 p2)
          (call-with-values
            (lambda () (dxdy p1 p2))
            (lambda (dx dy) (sqrt (+ (* dx dx) (* dy dy))))))) 

      (define segment-slope
        (lambda (p1 p2)
          (call-with-values
            (lambda () (dxdy p1 p2))
            (lambda (dx dy) (/ dy dx))))) 

      (segment-length '(1 . 4) '(4 . 8)) <graphic> 5
      (segment-slope '(1 . 4) '(4 . 8)) <graphic> 4/3

      We can of course combine these to form one procedure that returns two values.

      (define describe-segment
        (lambda (p1 p2)
          (call-with-values
            (lambda () (dxdy p1 p2))
            (lambda (dx dy)
              (values
                (sqrt (+ (* dx dx) (* dy dy)))
                (/ dy dx)))))) 

      (describe-segment '(1 . 4) '(4 . 8)) <graphic> 5
                                           <graphic> 4/3



    Section 5.9. Eval


