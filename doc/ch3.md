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

        The nonlocal exit allows product to return immediately, without performing the pending multiplications, when a zero value is detected.

        Each of the continuation invocations above returns to the continuation while control remains within the procedure passed to call/cc.
        The following example uses the continuation after this procedure has already returned.

            (let ([x (call/cc (lambda (k) k))])
              (x (lambda (ignore) "hi"))) => "hi"

        The continuation captured by this invocation of call/cc may be described as "Take the value, bind it to x, and apply the value of x to the value of (lambda (ignore) "hi")."
        Since (lambda (k) k) returns its argument, x is bound to the continuation itself; 
        this continuation is applied to the procedure resulting from the evaluation of (lambda (ignore) "hi"). 
        This has the effect of binding x (again!) to this procedure and applying the procedure to itself. The procedure ignores its argument and returns "hi".

        The following variation of the example above is probably the most confusing Scheme program of its size;
        it might be easy to guess what it returns, but it takes some thought to figure out why.

            (((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!") => "HEY!"

        The value of the call/cc is its own continuation, as in the preceding example. 
        This is applied to the identity procedure (lambda (x) x), so the call/cc returns a second time with this value. 
        Then, the identity procedure is applied to itself, yielding the identity procedure. This is finally applied to "HEY!", yielding "HEY!".

        Continuations used in this manner are not always so puzzling. Consider the following definition of factorial 
        that saves the continuation at the base of the recursion before returning 1, by assigning the top-level variable retry.

            (define retry #f) 

            (define factorial
              (lambda (x)
                (if (= x 0)
                    (call/cc (lambda (k) (set! retry k) 1))
                    (* x (factorial (- x 1))))))
        With this definition, factorial works as we expect factorial to work, except it has the side effect of assigning retry.
            (factorial 4) => 24
            (retry 1) => 24
            (retry 2) => 48
        The continuation bound to retry might be described as "Multiply the value by 1, then multiply this result by 2, then multiply this result by 3, then multiply this result by 4." 
        If we pass the continuation a different value, i.e., not 1, we will cause the base value to be something other than 1 and hence change the end result.
            (retry 2) => 48
            (retry 5) => 120
        This mechanism could be the basis for a breakpoint package implemented with call/cc;
        each time a breakpoint is encountered, the continuation of the breakpoint is saved so that
        the computation may be restarted from the breakpoint (more than once, if desired).

        The traditional style of programming with subroutines provides a programming model that goes like this:

            make a note of what you were doing and what values your local variables had in some sort of temporary storage, aka “the stack”
            transfer control to a subroutine until it returns
            consult your notes; pick up where you left off, now knowing the result of the subroutine if there was one.

        CPS is a style of programming in which there are no “subroutines” per se and no “returns”.
        Instead, the last thing that the current function does is call the next function,
        passing the result of the current function to the next function. 
        Since no function ever “returns” or does work after it calls the next function,
        there’s no need to keep track of where you’ve been.
        It doesn’t matter where you were because you’re never coming back there.
        In order to ensure that things happen in the desired order, 
        when calling the new function you typically pass a “continuation”, 
        which is itself a function that executes “everything that comes next”.

        I showed a way to hack this up in JScript. You make every function take a continuation.
        New continuations can be built as needed out of nested anonymous functions.
        Any time that you would have called a subroutine,
        instead you make a continuation that represents the work you still have yet to do,
        and logically pass that to the subroutine, so that it can execute it for you.
        In order to do this without consuming any stack in JScript, 
        you can do this by passing the continuation to some sort of “orchestration” code that keeps track of what has to happen next.
        The orchestrator just sits there in a loop, dispatching the current continuation with the last-computed result.
        In languages that do not support CPS natively that’s pretty much the best you can do if you want to do full-on CPS programming without the consumption of stack.

        CPS is interesting and useful because it has a lot of nice properties,
        many of which I did not explain in my first series of articles.
        I presented CPS solely as a way to deal with the problem of deep recursions;
        since there are no subroutine calls that ever return,
        there is no need to consume call stack.
        But CPS is about much more than just that. 
        In particular, one of the things CPS lets us do is build new control flow primitives into a language by implementing control flow as methods.
        That might sound crazy, but let’s take a look at a very simple example of how we might build a control flow out of continuations.

        Consider for example the ?: conditional operator.
        It makes a decision about what happens next and therefore is a form of control flow.
        Suppose we have methods string M(int x), bool B(), int C(), and int D(). We might have this fragment somewhere in our program:
            M(B() ? C() : D())
        Suppose now that the C# language did not have the ?: operator and you wanted to implement it as a library method call. You can’t just go:
            T Conditional<T>(bool b, T consequence, T alternative)
            {
                if (b) return consequence; else return alternative;
            }
        because now the consequence and alternative are evaluated eagerly, instead of lazily. But we could do this:
            T Conditional<T>(bool b, Func<T> consequence, Func<T> alternative)
            {
              if (b) return consequence(); else return alternative();
            }
        And now call
            M(Conditional(B(), ()=>C(), ()=>D()))

        We have our conditional operator implemented as a library method.

        Now suppose we wanted to do the conditional operator in CPS because… because we’re gluttons for punishment I guess.
        In CPS we have some continuation; something is going to happen after the call to M. 
        Let’s call that the “current continuation”, whatever it is. How we obtain it is not important, just suppose we have it in hand.

        We need to rewrite B so that it takes a continuation that accepts a bool.
        “A continuation that takes a bool” sounds an awful lot like an Action<bool>, so let’s assume that we can rewrite bool B() to be void B(Action<bool>).

        What is the continuation of B, the “thing that happens after”? Let’s take it one step at a time.

            B(b=>M(Conditional(b, ()=>C(), ()=>D)))

        We have B in CPS, but the lambda passed to B is not in CPS because it does two things: calls Conditional, and calls M.
        To be in CPS it has to call something as the last thing it does. 
        Let’s repeat the analysis we just did for B. M needs to take an int and an Action<string>. C and D need to take Action<int>. 
        What about Conditional? It still needs to lazily evaluate its arguments, 
        but calling those lambdas cannot return a value either; rather, they have to take continuations too:
            B(b=>Conditional(b, c=>C(c), c=>D(c), t=>M(t, currentContinuation)))

        Conditional now looks like this:

            void Conditional<T> (bool condition, Action<Action<T>> consequence, Action<Action<T>> alternative, Action<T> continuation)
            {
              if (condition) consequence(continuation) ; else alternative(continuation); 
            }
        Summing up: B executes and passes its bool result to a lambda.
        That lambda passes b and three different lambdas to Conditional.
        Conditional decides which of the first two delegates to pass the third delegate – the continuation – to.
        Suppose it chooses the alternative. The alternative, D, runs and passes its result to the continuation,
        which is t=>M(currentContinuation, t). M then does its thing with integer t,
        and invokes whatever the current continuation of the original call was, and we’re done.

        We’re getting hung up on the stack management aspects of recursive programming. Why do we need a stack at all? What purpose does it serve?

        If you’re like most developers, you probably learned about subroutines and functions at an early age. 
        The idea is pretty straightforward.  You stop what you’re doing right now, go perform some other task, 
        and then pick up where you left off, possibly using data computed by the other task.  
        But this idea of “pick up where you left off” means that you have to have some mechanism for storing information about 
        what you were doing and where to pick it up. A stack is a natural data structure for that, 
        so we’ve been thinking a lot about stacks lately when we consider how to operate on recursive data structures.

        But what if we simply denied the whole premise of functions? What if we said that no function was ever allowed to return?  
        It either terminates the program, or itself calls another function.

        You might be thinking that clearly the cure is worse than the disease.  
        I mean, sure, now you don’t have to keep track of what you were doing, 
        but that’s because you’re never going to get back there!  
        How is it possible to get any work done at all if every time you try to call a procedure for a little help, 
        it takes over control and never comes back to you?

        This sounds like crazy talk, but let’s think about the consequences of removing all returns from the language. Three things are clear.

        First, if you yourself write a function then calling another function has to be the last thing you do. 
        Control is never coming back, so there’s no point in having any code after a function call.

        Second, suppose you have more work to do after you call a function foo. 
        Foo is in the same position as you — the last thing it does will either be a function call or terminating the program. 
        Therefore, you need to put the work that needs to be done after foo into function bar, and make for darn sure that foo calls bar rather than terminating!

        But if you didn’t write foo, how do you know that foo is going to call bar when its done?  You need to somehow tell foo to call bar, and hope that foo cooperates.

        Third, if someone calls your function then they’re in the same boat — it’s the last thing they’re going to do, ever, because you’re not coming back. 
        You need to find some way to allow your caller to tell you what to do when you’re done your work.

        This idea of functions never returning, and callers passing their functions information about what needs to be done next, 
        is a bizarre but powerful style of programming called Continuation Passing Style. 
        The “information about what to do next” is called a “continuation”, which is passed from function to function, hence the name.

        Many languages support CPS natively — Scheme, Ruby and Rhino, for example, all support CPS.  
        To get it working in JScript will take some doing, but when we’re done there will be no explicit stack at all in our formerly recursive program.

        In the next episode I’m going to write our tree depth program in a mix of CPS and regular procedural programming. 
        (To be purely CPS we’d write CPS versions of the addition operators, the Math.max function, and so on, but I’m not going to go there  — that’s overkill.)  
        But before we get into the tree depth program let’s get a little more familiar with CPS.

        Suppose we had this fragment of a JScript program:


            function foo(x)
            {
              var y = bar();
              blah(x,y); 
            }
            function bar()
            {
              return 123;
            }
            function blah(a, b)
            {
              print(a+b);
            }
            foo(1);

        I hope you agree that this is a very straightforward program. How would we do this in CPS?  
        Well, first of all, every function would have to take an extra argument for the continuation. 
        Our program calls foo and then terminates, so the continuation of the call to foo is “terminate the program”.  
        Let’s presume that we have a magic function that does that.

            function foo(x, cfoo)
            {
              // UNDONE: rewrite foo in CPS
            }
            foo(1, terminate);

        Let’s reason about foo from end to start. foo wants to ensure that three things happen in this order:

            bar runs
            blah runs
            foo‘s continuation from its caller runs

        But blah is never going to return.  
        Therefore we’ll make it blah‘s responsibility to call foo‘s continuation when it is done. 
        We’ll rewrite blah so that it does CPS, so that we can say:

            function foo(x, cfoo)
            {
              var y = bar();
              blah(x,y,cfoo); 
            }

        OK, super, we’ve taken care of our responsibility to our caller by foisting it off onto blah.  
        But now we have an additional problem. bar is never going to return, so we’re never going to call blah!  
        We’ve both failed in our responsibility to our caller and haven’t done the work we need to do.

        We have an additional problem. bar was going to return a value that we were going to use. 
        But we’ve just removed returns from the language! Let’s solve both of these problems.  
        We’ll say that bar takes a continuation and passes the value that it was going to return as an argument to its continuation. 
        That’s functionally the same thing, right? Before, you return the value to “whatever you were going to do next”, 
        now you pass the value to “whatever you were going to do next”, same thing.

            function foo(x, afterfoo)
            {
                function foocontinuation(y)
                {
                    blah(x,y,afterfoo); 
                }
                bar(foocontinuation);
            }
            function bar(afterbar)
            {
                afterbar(123);
            }
            function blah(a, b, afterblah)
            {
                print(a+b, afterblah); // print also rewritten in CPS
            }

            foo(1, terminate);

        foocontinuation is a closure, so it keeps track of what the values of x and afterfoo were when it was passed to bar.  So we’re all set here. 

            The global code passes 1, terminate to foo  
            foo passes foocontinuation to bar
            bar passes 123 to foocontinuation  
            foocontinuation passes 1, 123, terminate to blah
            blah adds together its arguments and passes 124, terminate to print
            print presumably prints out 124 and calls terminate, and we’re done

        Not once did any function return in there, and we did everything in the right order.

        Now of course in reality, JScript does not know that none of these functions are going to return. 
        Nor is JScript smart enough to realize that even if they did return, 
        none of these functions now does anything after the subroutine call, 
        and therefore keeping track of the old frames on the stack is unnecessary, since they’re never going to be read from again. 
        If it did, we could totally write programs in this style and never worry about running out of stack space, but unfortunately, it doesn’t.

        But just as we helped JScript along by writing our own explicit frame stack, we can help it along by writing our own CPS system. Stay tuned!



        Continuations may be used to implement various forms of multitasking.
        The simple "light-weight process" mechanism defined below allows multiple computations to be interleaved.
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
        The following light-weight processes cooperate to print an infinite sequence of lines containing "hey!".
            (lwp (lambda () (let f () (pause) (display "h") (f))))
            (lwp (lambda () (let f () (pause) (display "e") (f))))
            (lwp (lambda () (let f () (pause) (display "y") (f))))
            (lwp (lambda () (let f () (pause) (display "!") (f))))
            (lwp (lambda () (let f () (pause) (newline) (f))))
            (start) => hey!
                       hey!
                       hey!
                       hey!
                       ...
