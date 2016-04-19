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



    Section 5.3. Conditionals



    Section 5.4. Recursion and Iteration



    Section 5.5. Mapping and Folding



    Section 5.6. Continuations



    Section 5.7. Delayed Evaluation



    Section 5.8. Multiple Values



    Section 5.9. Eval




