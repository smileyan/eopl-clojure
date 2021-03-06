
#lang eopl

1.1 Recursively Specified Data
   1.1.1 Inductive Specification

    ; top-down
    ; Definition 1.1.1 A natural number n is in S iff
    ; 1. n = 0, or
    ; 2. n - 3 is in S.

    ; in-S? : N -> Bool
    ; usage
    (define in-S?
    (lambda (n)
        (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

    ; botton up
    ; Definition 1.1.2 Define the set S to be the smallest set contained in N and satisfy-
    ; ing the following two properties:
    ; 1. 0 is in S, and
    ; 2. if n is in S, then n + 3 is in S.

    ; rules-of-inference
            ;    _________
            ;    0 is in S

            ;    n is in S
            ; ---------------
            ; (n + 3) is in S

    ; Definition 1.1.3(list of integers, top-down) A Scheme list is a list of integers
    ; iff either
    ; 1. it is a empty list, or
    ; 2. it is a pair whose car is an integer and cdr is a list of integers.

    ; Definition 1.1.4(list of integers, botton-down) The set of List-of-Int is the smallest
    ; set of Scheme lists satisfying the following two properties:
    ; 1. () is in List-of-Int, and
    ; 2. if n is in Int and L is in List-of-Int, then (n . l) is in List-of-Int.

    ; Definition 1.1.5(list of integers, rules of inference)
    ;                       0 is in List-of-Int
                        
    ;                   n is in Int    l is in List-of-Int
    ;                   ----------------------------------
    ;                         (n . l) is in List-of-Int

   1.1.2 Definition Sets Using Grammars

         List-of-Int ::= ()
         List-of-Int ::= (Int . List-of-Int)
 
    1. Nonterminal Symbols
    2. Terminal Symbols
    3. Productions

         List-of-Int ::= ()
                     ::= (Int . List-of-Int)

         List-of-Int ::= () | (Int . List-of-Int)

    Definition 1.1.6 (s-list, s-exp)
                       S-list ::= ({S-exp}*)
                       S-exp  ::= Symbol | S-list

    Definition 1.1.7 (binary tree)
                       Bintree ::= Int | (Symbol Bintree Bintree)

    Definition 1.1.8 (lambda expression)
                       LcExp ::= Identifier
                             ::= (lambda (Identifier) LcExp)
                             ::= (LcExp LcExp)
      where an identifier is any symbol other than lambda.


   1.1.3 Induction
      1. to prove theorems about members of the set and 
      2. to write programs that manipulate them.

    Theorem 1.1.1 Let t be a binary tree, as definition 1.1.7. Then t contains
    an odd number of nodes.

    ___________________________________________________________________________________
                            Proof by Structual Induction
    To prove that a proposition IH(s) is true for all structures s, prove the follow-
    ing:
        1. IH is true on simple structures (those without substructures).
        2. If IH is true on the substructures of s, then it is true on s itself.
    ___________________________________________________________________________________


1.2 Deriving Recursive Programs
    1.2.0 The Smaller-Subproblem Principle
     _____________________________________________________________________________
                     The Smaller-Subproblem Principle
      If we can reduce a problem to a smaller subproblem, we can call the procedure
      that solves the problem to solve the subproblem.
     ______________________________________________________________________________

    1.2.1 list-length

        > (length '(a b c))
        3
        > (length '((x) ()))
        2

        list-length : List -> Int
        usage: (list-length l) = the length of l
        (define list-length
        (lambda (lst)
            (if (null? lst)
            0
            (+ 1 (list-length (cdr lst))))))

        (list-length '(a (b c) d))
        = (+ 1 list-length '((b c) d))
        = (+ 1 (+ 1 list-length '(d)))
        = (+ 1 (+ 1 (+ 1 list-length '())))
        = (+ 1 (+ 1 (+ 1 0)))
        = 3

    1.2.2 nth-element
        > (list-ref '(a b c) 1)
        b

        nth-element : List * Int -> SchemeVal
        usage: (nth-element lst n) = the n-th element of lst
        (define nth-element
            (lambda (lst n)
            (if (null? lst)
                (report-list-too-short n)
                (if (zero? n)
                (car lst)
                (nth-element (cdr lst) (- n 1))))))
        
        (define report-list-too-short
            (lambda (n)
            (eopl:error 'nth-element
                "List too short by ~s elements.~%" (+ n 1))))
        
            (nth-element '(a b c d e) 3)
        = (nth-element '(b c d e) 2)
        = (nth-element '(c d e) 1)
        = (nth-element '(d e) 0)
        = d

    1.2.3 remove-first

        > (remove-first 'a '(a b c))
        (b c)
        > (remove-first 'b '(e f g))
        (e f g)
        > (remove-first 'a4 '(c1 a4 c1 a4))
        (c1 c1 a4)
        > (remove-first 'x '())
        ()
        
                List-of-Symbol ::= () | (Symbol . List-of-Symbol)
        
        remove-first : Sym * Listof(Sym) -> Listof(Sym)
        usage:  (remove-first s los) returns a list with 
                the same elements arranged in the same
                order as los, except that the first
                occurrence of the symbol s is removed.
        (define remove-first
            (lambda (s los)
            (if (null? los)
                '()
                (if (eqv? (car los) s)
                (cdr los))
                (cons (car los) (remove-first s (cdr los))))))

    1.2.4 occurs-free?

        > (occurs-free? 'x 'x)
        #t
        > (occurs-free? 'x 'y)
        #f
        > (occurs-free? 'x '(lambda (x) (x y)))
        #f
        > (occurs-free? 'x '(lambda (y) (x y)))
        #t
        > (occurs-free? 'x '((lambda (x) x) (x y)))
        #t
        > (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))
        #t
                   LcExp ::= Identifier
                         ::= (lambda (Identifier) LcExp)
                         ::= (LcExp LcExp)
        
        occurs-free? : Sym * LcExp -> Bool
        usage:      returns #t if the symbol var occurs free
                    in exp, otherwise returns #f.
        (define occurs-free?
          (lambda (var exp)
            (cond
              ((symbol? exp) (eqv? var exp))
              ((eqv? (car exp) 'lambda)
               (and
                 (not (eqv? var (car (cadr exp))))
                 (occurs-free? var (caddr exp))))
              (else
                (or
                  (occurs-free? var (car exp))
                  (occurs-free? var (cadr exp)))))))

    1.2.5 subst
        
        > (subst 'a 'b '((b c) (b () d)))
        ((a c) (a () d))
        
        S-list ::= ({S-exp}*)
        S-exp  ::= Symbol | S-list
        
        S-list ::= ()
               ::= (S-exp . S-list)
        S-exp  ::= Symbol | S-list
        
        subst : Sym * Sym * S-list -> S-list
        (define subst
          (lambda (new old slist)
            (if (null? slist)
              '()
              (cons
                (subst-in-s-exp new old (car slist))
                (subst new old (cdr slist))))))
        
        subst-in-s-exp : Sym * Sym * S-exp -> S-exp
        (define subst-in-s-exp
          (lambda (new old sexp)
            (if (symbol? sexp)
              (if (eqv? sexp old) new sexp)
              (subst new old sexp))))
        
        _____________________________________________________________________________
                               Follow the Grammar!
         When defining a procedure that operates on inductively defined data, the
         structure of the program should be patterned after the structure of the data.
        _____________________________________________________________________________
        
        Write one procedure for each nonterminal in the grammar. The procedure
        will be responsible for handling the data corresponding to that nontermi-
        nal, and nothing else.
        
        In each procedure, write one alternative for each production correspond-
        ing to that nonterminal. You may need additional case structure, but this
        will get you started. For each nonterminal that appears in the right-hand
        side, write a recursive call to the procedure for that nonterminal.

1.3 Auxiliary Procedures and Context Arguments
