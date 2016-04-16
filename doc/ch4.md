Chapter 4. Procedures and Variable Bindings
        Procedures and variable bindings are the fundamental building blocks of Scheme programs. 
        This chapter describes the small set of syntactic forms whose primary purpose is to create procedures and manipulate variable bindings. 
        It begins with the two most fundamental building blocks of Scheme programs: variable references and lambda expressions, 
        and continues with descriptions of the variable binding and assignment forms such as define, letrec, let-values, and set!.

        Various other forms that bind or assign variables for which the binding or assignment is not the primary purpose (such as named let) are found in Chapter 5.

    Section 4.1. Variable References

        syntax: variable 
        returns: the value of variable

        Any identifier appearing as an expression in a program is a variable if a visible variable binding for the identifier exists, e.g., 
        the identifier appears within the scope of a binding created by define, lambda, let, or some other variable-binding construct.

            list => #<procedure>
            (define x 'a)
            (list x x) => (a a)
            (let ([x 'b])
              (list x x)) => (b b)
            (let ([let 'let]) let) => let

        It is a syntax violation for an identifier reference to appear within a library form or top-level program if it is not bound as a variable, 
        keyword, record name, or other entity. Since the scope of the definitions in a library, top-level program, lambda, or other local body is the entire body, 
        it is not necessary for the definition of a variable to appear before its first reference appears, 
        as long as the reference is not actually evaluated until the definition has been completed. So, for example, the reference to g within the definition of f below

            (define f
              (lambda (x)
                (g x)))
            (define g
              (lambda (x)
                (+ x x)))

        is okay, but the reference to g in the definition of q below is not.

            (define q (g 3))
            (define g
              (lambda (x)
                (+ x x)))

    Section 4.2. Lambda

        syntax: (lambda formals body1 body2 ...) 
        returns: a procedure 
        libraries: (rnrs base), (rnrs)

        The lambda syntactic form is used to create procedures. Any operation that creates a procedure or establishes local variable bindings is ultimately defined in terms of lambda or case-lambda.

        The variables in formals are the formal parameters of the procedure, and the sequence of subforms body1 body2 ... is its body.

        The body may begin with a sequence of definitions, in which case the bindings created by the definitions are local to the body. 
        If definitions are present, the keyword bindings are used and discarded while expanding the body, 
        and the body is expanded into a letrec* expression formed from the variable definitions and the remaining expressions, 
        as described on page 292. The remainder of this description of lambda assumes that this transformation has taken place, 
        if necessary, so that the body is a sequence of expressions without definitions.

        When the procedure is created, the bindings of all variables occurring free within the body, excluding the formal parameters, are retained with the procedure. 
        Subsequently, whenever the procedure is applied to a sequence of actual parameters, the formal parameters are bound to the actual parameters, 
        the retained bindings are restored, and the body is evaluated.

        Upon application, the formal parameters defined by formals are bound to the actual parameters as follows.

            If formals is a proper list of variables, e.g., (x y z), each variable is bound to the corresponding actual parameter. 
              An exception with condition type &assertion is raised if too few or too many actual parameters are supplied.
            If formals is a single variable (not in a list), e.g., z, it is bound to a list of the actual parameters.
            If formals is an improper list of variables terminated by a variable, e.g., (x y . z), 
              each variable but the last is bound to the corresponding actual parameter. 
              The last variable is bound to a list of the remaining actual parameters. An exception with condition type &assertion is raised if too few actual parameters are supplied.

        When the body is evaluated, the expressions in the body are evaluated in sequence, and the procedure returns the values of the last expression.

        Procedures do not have a printed representation in the usual sense. Scheme systems print procedures in different ways; this book uses the notation #<procedure>.

            (lambda (x) (+ x 3)) <graphic> #<procedure>
            ((lambda (x) (+ x 3)) 7) <graphic> 10
            ((lambda (x y) (* x (+ x y))) 7 13) <graphic> 140
            ((lambda (f x) (f x x)) + 11) <graphic> 22
            ((lambda () (+ 3 4))) <graphic> 7 

            ((lambda (x . y) (list x y))
            28 37) <graphic> (28 (37))
            ((lambda (x . y) (list x y))
            28 37 47 28) <graphic> (28 (37 47 28))
            ((lambda (x y . z) (list x y z))
            1 2 3 4) <graphic> (1 2 (3 4))
            ((lambda x x) 7 13) <graphic> (7 13)

    Section 4.3. Case-Lambda

        A Scheme lambda expression always produces a procedure with a fixed number of arguments or with an indefinite number of arguments greater than or equal to a certain number. In particular,

            (lambda (var1 ... varn) body1 body2 ...)

        accepts exactly n arguments,

            (lambda r body1 body2 ...)
        accepts zero or more arguments, and
            (lambda (var1 ... varn . r) body1 body2 ...)
        accepts n or more arguments.

        lambda cannot directly produce, however, a procedure that accepts, say, either two or three arguments. 
        In particular, procedures that accept optional arguments are not supported directly by lambda. 
        The latter form of lambda shown above can be used, in conjunction with length checks and compositions of car and cdr, 
        to implement procedures with optional arguments, though at the cost of clarity and efficiency.

        The case-lambda syntactic form directly supports procedures with optional arguments as well as procedures with fixed or indefinite numbers of arguments. 
        case-lambda is based on the lambda* syntactic form introduced in the article "A New Approach to Procedures with Variable Arity" [11].

        syntax: (case-lambda clause ...) 
        returns: a procedure 
        libraries: (rnrs control), (rnrs)

        A case-lambda expression consists of a set of clauses, each resembling a lambda expression. Each clause has the form below.

            [formals body1 body2 ...]

        The formal parameters of a clause are defined by formals in the same manner as for a lambda expression. 
        The number of arguments accepted by the procedure value of a case-lambda expression is determined by the numbers of arguments accepted by the individual clauses.

        When a procedure created with case-lambda is invoked, the clauses are considered in order. 
        The first clause that accepts the given number of actual parameters is selected, the formal parameters defined by its formals 
        are bound to the corresponding actual parameters, and the body is evaluated as described for lambda above. 
        If formals in a clause is a proper list of identifiers, then the clause accepts exactly as many actual parameters as there are formal parameters (identifiers) in formals. 
        As with a lambda formals, a case-lambda clause formals may be a single identifier, in which case the clause accepts any number of arguments, 
        or an improper list of identifiers terminated by an identifier, in which case the clause accepts any number of arguments greater than or equal to 
        the number of formal parameters excluding the terminating identifier. If no clause accepts the number of actual parameters supplied, an exception with condition type &assertion is raised.

        The following definition for make-list uses case-lambda to support an optional fill parameter.

            (define make-list
              (case-lambda
                [(n) (make-list n #f)]
                [(n x)
                  (do ([n n (- n 1)] [ls '() (cons x ls)])
                      ((zero? n) ls))]))

        The substring procedure may be extended with case-lambda to accept either no end index, 
        in which case it defaults to the end of the string, or no start and end indices, in which case substring is equivalent to string-copy:

            (define substring1
              (case-lambda
                [(s) (substring1 s 0 (string-length s))]
                [(s start) (substring1 s start (string-length s))]
                [(s start end) (substring s start end)]))













