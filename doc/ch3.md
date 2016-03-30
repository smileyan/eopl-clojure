# Chapter 3. Going Further
    Section 3.1. Syntactic Extension
        <program>	           --> 	<form>*
        <form>	               -->  <definition> | <expression>
        <definition>           -->  <variable definition> | (begin <definition>*)
        <variable definition>  -->	(define <variable> <expression>)
        <expression>           -->  <constant>
                                |	<variable>
                                |	(quote <datum>)
                                |	(lambda <formals> <expression> <expression>*)
                                |	(if <expression> <expression> <expression>)
                                |	(set! <variable> <expression>)
                                |	<application>
        <constant>	           -->	<boolean> | <number> | <character> | <string>
        <formals>              -->	<variable>
                                |	(<variable>*)
                                |	(<variable> <variable>* . <variable>)
        <application>	       -->	(<expression> <expression>*)
        Syntactic extensions are defined with define-syntax. define-syntax is
        similar to define, except that define-syntax associates a syntactic
        transformation procedure, or transformer, with a keyword (such as let),
        rather than associating a value with a variable. Here is how we might
        define let with define-syntax.

        (define-syntax let
          (syntax-rules ()
            [(_ ((x e) ...) b1 b2 ...)
             ((lambda (x ...) b1 b2 ...) e ...)]))

        The identifier appearing after define-syntax is the name, or keyword,
        of the syntactic extension being defined, in this case let. The
        syntax-rules form is an expression that evaluates to a transformer.
        The item following syntax-rules is a list of auxiliary keywords and
        is nearly always (). An example of an auxiliary keyword is the else
        of cond. (Other examples requiring the use of auxiliary keywords are
        given in Chapter 8.) Following the list of auxiliary keywords is a
        sequence of one or more rules, or pattern/template pairs. Only one
        rule appears in our definition of let. The pattern part of a rule
        specifies the form that the input must take, and the template
        specifies to what the input should be transformed.
        
        The pattern should always be a structured expression whose first
        element is an underscore ( _ ). (As we will see in Chapter 8, the use
        of _ is only a convention, but it is a good one to follow.) If more
        than one rule is present, the appropriate one is chosen by matching
        the patterns, in order, against the input during expansion. It is a
        syntax violation if none of the patterns match the input.
        
        Identifiers other than an underscore or ellipsis appearing within a
        pattern are pattern variables, unless they are listed as auxiliary
        keywords. Pattern variables match any substructure and are bound to
        that substructure within the corresponding template. The notation pat
        ... in the pattern allows for zero or more expressions matching the
        ellipsis prototype pat in the input. Similarly, the notation expr ...
        in the template produces zero or more expressions from the ellipsis
        prototype expr in the output. The number of pats in the input 
        determines the number of exprs in the output; in order for this to work
        , any ellipsis prototype in the template must contain at least one
        pattern variable from an ellipsis prototype in the pattern.

        The single rule in our definition of let should be fairly 
        self-explanatory, but a few points are worth mentioning. First, the
        syntax of let requires that the body contain at least one form; hence,
        we have specified b1 b2 ... instead of b ..., which might seem more
        natural. On the other hand, let does not require that there be at least
        one variable/value pair, so we were able to use, simply, (x e) .... 
        Second, the pattern variables x and e, though together within the same
        prototype in the pattern, are separated in the template; any sort of
        rearrangement or recombination is possible. Finally, the three pattern
        variables x, e, and b2 that appear in ellipsis prototypes in the 
        pattern also appear in ellipsis prototypes in the template. This is not
        a coincidence; it is a requirement. In general, if a pattern variable
        appears within an ellipsis prototype in the pattern, it cannot appear
        outside an ellipsis prototype in the template.
        
        (define-syntax and
          (syntax-rules ()
            [(_) #t]
            [(_ e) e]
            [(_ e1 e2 e3 ...)
             (if e1 (and e2 e3 ...) #f)]))
        This definition is recursive and involves more than one rule. Recall that
        (and) evaluates to #t; the first rule takes care of this case. The second
        and third rules specify the base case and recursion steps of the 
        recursion and together translate and expressions with two or more 
        subexpressions into nested if expressions. For example, (and a b c)
        expands first into
        
        (if a (and b c) #f)
        then
        
        (if a (if b (and c) #f) #f)
        
        and finally
        (if a (if b c #f) #f)