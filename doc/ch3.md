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
        
        (define-syntax and ; incorrect!
          (syntax-rules ()
            [(_) #t]
            [(_ e1 e2 ...)
             (if e1 (and e2 ...) #f)]))
        (and (not (= x 0)) (/ 1 x))
        (if (not (= x 0)) (and (/ 1 x)) #f)
          (if (not (= x 0)) (and (/ 1 x)) #f)
        
        (define-syntax or
          (syntax-rules ()
            [(_) #f]
            [(_ e) e]
            [(_ e1 e2 e3 ...)
             (let ([t e1])
               (if t t (or e2 e3)))])) 
    Section 3.2. More Recursion
        (let ([sum (lambda (ls)
                     (if (null? ls)
                         0
                         (+ (car ls) (sum (cdr ls)))))])
          (sum '(1 2 3 4 5)))
         it will probably raise an exception with a message to the effect that sum is undefined.
         This is because the variable sum is visible only within the body of the let expression
         and not within the lambda expression whose value is bound to sum. We can get around
         this problem by passing the procedure sum to itself as follows.
        
        (let ([sum (lambda (sum ls)
                     (if (null? ls)
                         0
                         (+ (car ls) (sum sum (cdr ls)))))])
          (sum sum '(1 2 3 4 5))) => 15
         This works and is a clever solution, but there is an easier way, using letrec. Like let,
         the letrec syntactic form includes a set of variable-value pairs, along with a sequence
         of expressions referred to as the body of the letrec.
        (letrec ((var expr) ...) body1 body2 ...)
         Unlike let, the variables var ... are visible not only within the body of the letrec but
          also within expr .... Thus, we can rewrite the expression above as follows.
        (letrec ([sum (lambda(ls)
                        (if (null? ls)
                            0
                            (+ (car ls) (sum (cdr ls)))))])
          (sum '(1 2 3 4 5))) => 15
         Using letrec, we can also define mutually recursive procedures, such as the procedures
         even? and odd? that were the subject of Exercise 2.8.6.
         (letrec ([even?
                   (lambda (x)
                     (or (= x 0)
                         (odd? (- x 1))))]
                  [odd?
                   (lambda (x)
                     (and (not (= x 0))
                          (even? (- x 1))))])
           (list (even? 20) (odd? 20))) => (#t #f)
         In a letrec expression, expr ... are most often lambda expressions, though this need not be
         the case. One restriction on the expressions must be obeyed, however. It must be possible to
         evaluate each expr without evaluating any of the variables var .... This restriction is 
         always satisfied if the expressions are all lambda expressions, since even though the 
         variables may appear within the lambda expressions, they cannot be evaluated until the 
         resulting procedures are invoked in the body of the letrec. The following letrec expression 
         obeys this restriction.
         (letrec ([f (lambda () (+ x 2))]
                  [x 1])
           (f)) => 3
         (letrec ([y (+ x 2)]
                  [x 1])
           y)
         We can use letrec to hide the definitions of "help" procedures so that they do not clutter
          the top-level namespace. This is demonstrated by the definition of list? below, which
           follows the "hare and tortoise" algorithm outlined in Exercise 2.9.8.
         (define list?
           (lambda (x)
             (letrec ([race
                       (lambda (h t)
                         (if (pair? h)
                             (let ([h (cdr h)])
                               (if (pair? h)
                                   (and (not (eq? h t))
                                        (race (cdr h) (cdr t)))
                                   (null? h)))
                             (null? h)))])
             (race x x))))
         Just as let can be expressed as a simple direct application of a lambda expression to arguments, named let can be expressed as the application of a
         recursive procedure to arguments. A named let of the form

         (let name (var expr) ...
           body1 body2 ...)
         
         can be rewritten in terms of letrec as follows.

         ((letrec ((name (lambda (var ...) body1 body2 ...)))
            name)
           expr ...)
         Alternatively, it can be rewritten as
         
         (letrec ((name (lambda (var ...) body1 body2 ...)))
           (name expr ...))
         
         As we discussed in Section 2.8, some recursion is essentially iteration and executes as such.
         When a procedure call is in tail position (see below) with respect to a lambda expression, 
         it is considered to be a tail call, and Scheme systems must treat it properly, as a "goto" or 
         jump. When a procedure tail-calls itself or calls itself indirectly through a series of tail 
         calls, the result is tail recursion. Because tail calls are treated as jumps, tail recursion 
         can be used for indefinite iteration in place of the more restrictive iteration constructs 
         provided by other programming languages, without fear of overflowing any sort of recursion stack.

         A call is in tail position with respect to a lambda expression if its value is returned directly
         from the lambda expression, i.e., if nothing is left to do after the call but to return from the
         lambda expression. For example, a call is in tail position if it is the last expression in the
         body of a lambda expression, the consequent or alternative part of an if expression in tail
         position, the last subexpression of an and or or expression in tail position, the last expression
         in the body of a let or letrec in tail position, etc. Each of the calls to f in the expressions
         below are tail calls, but the calls to g are not.

         (lambda () (f (g)))
         (lambda () (if (g) (f) (f)))
         (lambda () (let ([x 4]) (f)))
         (lambda () (or (g) (f)))
        In each case, the values of the calls to f are returned directly, whereas the calls to g are not.

        Recursion in general and named let in particular provide a natural way to implement many algorithms,
        whether iterative, recursive, or partly iterative and partly recursive
        ; the programmer is not burdened with two distinct mechanisms.

        The following two definitions of factorial use named let expressions to compute the factorial, n!,
        of a nonnegative integer n. The first employs the recursive definition n! = n × (n - 1)!, where 0! is defined to be 1.
        (define factorial
          (lambda (n)
            (let fact ([i n])
              (if (= i 0))
                  1
                  (* i (fact (- i 1)))))
        (define factorial
          (lambda (n)
            (let fact ([i n] [a 1])
              (if (= i 0)
                  a
                  (fact (- i 1) (* a i))))))
        A similar problem is to compute the nth Fibonacci number for a given n.
        The Fibonacci numbers are an infinite sequence of integers, 0, 1, 1, 2, 3, 5, 8, etc.,
        in which each number is the sum of the two preceding numbers in the sequence.
        A procedure to compute the nth Fibonacci number is most naturally defined recursively as follows.
        (define fibonacci
          (lambda (n)
            (let fib ([i n])
              (cond
                [(= i 0) 0]
                [(= i 1) 1]
                [else (+ (fib (- i 1)) (fib (- i 2)))]))))
        This solution requires the computation of the two preceding Fibonacci numbers at each step and 
        hence is doubly recursive. For example, to compute (fibonacci 4) requires the computation of both 
        (fib 3) and (fib 2), to compute (fib 3) requires computing both (fib 2) and (fib 1), and to compute 
        (fib 2) requires computing both (fib 1) and (fib 0). This is very inefficient, and it becomes 
        more inefficient as n grows. A more efficient solution is to adapt the accumulator solution of 
        the factorial example above to use two accumulators, a1 for the current Fibonacci number and a2 
        for the preceding one.
        (define fibonacci
          (lambda (n)
            (let fib ([i n] [a1 1] [a2 0])
              (if (= i 0)
                  a1
                  (fib (- i 1) (+ a1 a2) (a1))))))
        Here, zero is treated as a special case, since there is no preceding value. This allows us to use 
        the single base case (= i 1). The time it takes to compute the nth Fibonacci number using this 
        iterative solution grows linearly with n, which makes a significant difference when compared to 
        the doubly recursive version. To get a feel for the difference, try computing (fibonacci 35) 
        and (fibonacci 40) using both definitions to see how long each takes.

        We can also get a feel for the difference by looking at a trace for each on small inputs. 
        The first trace below shows the calls to fib in the non-tail-recursive version of fibonacci, with input 5.

        Notice how there are several calls to fib with arguments 2, 1, and 0. 
        The second trace shows the calls to fib in the tail-recursive version, again with input 5.
        
        Clearly, there is quite a difference.

        The named let examples shown so far are either tail-recursive or not tail-recursive. 
        It often happens that one recursive call within the same expression is tail-recursive while another is not. 
        The definition of factor below computes the prime factors of its nonnegative integer argument. 
        The first call to f is not tail-recursive, but the second one is.

        (define factor
          (lambda(n)
            (let f ([n n] [i 2])
              (cond
                [(>= i n) (list n)]
                [(integer? (/ n i))
                     (cons (i (f (/ n i) i)))]
                [else (f n (+ i 1))]))))
    Section 3.3. Continuations
        During the evaluation of a Scheme expression, the implementation must keep track of two things: (1) what to evaluate and (2) what to do with the value.
        Consider the evaluation of (null? x) within the expression below.

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
                (* 5 4)))
             -> 20

            (call/cc
              (lambda (k)
                (* 5 (k 4))))
             -> 4

            (+ 2
               (call/cc
                 (lambda (k)
                   (* 5 (k 4)))))
             -> 6
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
                        [(= (car ls ) 0) (break 0)]
                        [else (* (car ls) (f (cdr ls)))]))))))
            (product '(1 2 3 4 5))
             -> 120
            (product '(7 3 8 0 1 9 5))

