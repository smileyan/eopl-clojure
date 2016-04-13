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





