Chapter 6. Operations on Objects

    This chapter describes the operations on objects, including lists, numbers, characters, strings, vectors, bytevectors, symbols, booleans, hashtables, and enumerations. 
    The first section covers constant objects and quotation. 
    The second section describes generic equivalence predicates for comparing two objects and predicates for determining the type of an object. 
    Later sections describe procedures that deal primarily with one of the object types mentioned above. 
    There is no section treating operations on procedures, since the only operation defined specifically for procedures is application, 
    and this is described in Chapter 5. Operations on ports are covered in the more general discussion of input and output in Chapter 7. 
    A mechanism for defining new data types is described in Chapter 9.

  Section 6.1. Constants and Quotation

    syntax: constant 
    returns: constant

    constant is any self-evaluating constant, i.e., a number, boolean, character, string, or bytevector. Constants are immutable; see the note in the description of quote below.

    3.2 <graphic> 3.2
    #f <graphic> #f
    #\c <graphic> #\c
    "hi" <graphic> "hi"
    #vu8(3 4 5) <graphic> #vu8(3 4 5)

    syntax: (quote obj) 
    syntax: 'obj 
    returns: obj 
    libraries: (rnrs base), (rnrs)

    'obj is equivalent to (quote obj). The abbreviated form is converted into the longer form by the Scheme reader (see read).

    quote inhibits the normal evaluation rule for obj, allowing obj to be employed as data. 
    Although any Scheme object may be quoted, quotation is not necessary for self-evaluating constants, i.e., numbers, booleans, characters, strings, and bytevectors.

    Quoted and self-evaluating constants are immutable. That is, programs should not alter a constant via set-car!, string-set!, etc., and 
    implementations are permitted to raise an exception with condition type &assertion if such an alteration is attempted. 
    If an attempt to alter an immutable object is undetected, the behavior of the program is unspecified. 
    An implementation may choose to share storage among different constants to save space.

    (+ 2 3) <graphic> 5
    '(+ 2 3) <graphic> (+ 2 3)
    (quote (+ 2 3)) <graphic> (+ 2 3)
    'a <graphic> a
    'cons <graphic> cons
    '() <graphic> ()
    '7 <graphic> 7

    syntax: (quasiquote obj ...) 
    syntax: `obj 
    syntax: (unquote obj ...) 
    syntax: ,obj 
    syntax: (unquote-splicing obj ...) 
    syntax: ,@obj 
    returns: see below 
    libraries: (rnrs base), (rnrs)

    `obj is equivalent to (quasiquote obj), ,obj is equivalent to (unquote obj), and ,@obj is equivalent to (unquote-splicing obj). 
    The abbreviated forms are converted into the longer forms by the Scheme reader (see read).

    quasiquote is similar to quote, but it allows parts of the quoted text to be "unquoted." 
    Within a quasiquote expression, unquote and unquote-splicing subforms are evaluated, and everything else is quoted, i.e., left unevaluated. 
    The value of each unquote subform is inserted into the output in place of the unquote form, 
    while the value of each unquote-splicing subform is spliced into the surrounding list or vector structure. 
    unquote and unquote-splicing are valid only within quasiquote expressions.

    quasiquote expressions may be nested, with each quasiquote introducing a new level of quotation and each unquote or unquote-splicing taking away a level of quotation. 
    An expression nested within n quasiquote expressions must be within n unquote or unquote-splicing expressions to be evaluated.

    `(+ 2 3) <graphic> (+ 2 3) 

    `(+ 2 ,(* 3 4)) <graphic> (+ 2 12)
    `(a b (,(+ 2 3) c) d) <graphic> (a b (5 c) d)
    `(a b ,(reverse '(c d e)) f g) <graphic> (a b (e d c) f g)
    (let ([a 1] [b 2])
      `(,a . ,b)) <graphic> (1 . 2) 

    `(+ ,@(cdr '(* 2 3))) <graphic> (+ 2 3)
    `(a b ,@(reverse '(c d e)) f g) <graphic> (a b e d c f g)
    (let ([a 1] [b 2])
      `(,a ,@b)) <graphic> (1 . 2)
    `#(,@(list 1 2 3)) <graphic> #(1 2 3) 

    '`,(cons 'a 'b) <graphic> `,(cons 'a 'b)
    `',(cons 'a 'b) <graphic> '(a . b)

    unquote and unquote-splicing forms with zero or more than one subform are valid only in splicing (list or vector) contexts. 
    (unquote obj ...) is equivalent to (unquote obj) ..., and (unquote-splicing obj ...) is equivalent to (unquote-splicing obj) .... 
    These forms are primarily useful as intermediate forms in the output of the quasiquote expander. 
    They support certain useful nested quasiquotation idioms [3], such as ,@,@, 
    which has the effect of a doubly indirect splicing when used within a doubly nested and doubly evaluated quasiquote expression.

    `(a (unquote) b) <graphic> (a b)
    `(a (unquote (+ 3 3)) b) <graphic> (a 6 b)
    `(a (unquote (+ 3 3) (* 3 3)) b) <graphic> (a 6 9 b) 

    (let ([x '(m n)]) ``(a ,@,@x f)) <graphic> `(a (unquote-splicing m n) f)
    (let ([x '(m n)])
      (eval `(let ([m '(b c)] [n '(d e)]) `(a ,@,@x f))
            (environment '(rnrs)))) <graphic> (a b c d e f)

    unquote and unquote-splicing are auxiliary keywords for quasiquote. 
    It is a syntax violation to reference these identifiers except in contexts where they are recognized as auxiliary keywords.

  Section 6.2. Generic Equivalence and Type Predicates

    This section describes the basic Scheme predicates (procedures returning one of the boolean values #t or #f) for determining the type of an object or the equivalence of two objects. 
    The equivalence predicates eq?, eqv?, and equal? are discussed first, followed by the type predicates.

    procedure: (eq? obj1 obj2) 
    returns: #t if obj1 and obj2 are identical, #f otherwise 
    libraries: (rnrs base), (rnrs)

    In most Scheme systems, two objects are considered identical if they are represented internally by the same pointer value and 
    distinct (not identical) if they are represented internally by different pointer values, although other criteria, such as time-stamping, are possible.

    Although the particular rules for object identity vary somewhat from system to system, the following rules always hold.

      Two objects of different types (booleans, the empty list, pairs, numbers, characters, strings, vectors, symbols, and procedures) are distinct.
      
      Two objects of the same type with different contents or values are distinct.
      
      The boolean object #t is identical to itself wherever it appears, and #f is identical to itself wherever it appears, but #t and #f are distinct.
      
      The empty list () is identical to itself wherever it appears.
      
      Two symbols are identical if and only if they have the same name (by string=?).
      
      A constant pair, vector, string, or bytevector is identical to itself, 
      as is a pair, vector, string, or bytevector created by an application of cons, vector, string, make-bytevector, etc. 
      Two pairs, vectors, strings, or bytevectors created by different applications of cons, vector, string, make-bytevector, etc., are distinct. 
      One consequence is that cons, for example, may be used to create a unique object distinct from all other objects.
      
      Two procedures that may behave differently are distinct. A procedure created by an evaluation of a lambda expression is identical to itself. 
      Two procedures created by the same lambda expression at different times, or by similar lambda expressions, may or may not be distinct.

    eq? cannot be used to compare numbers and characters reliably. 
    Although every inexact number is distinct from every exact number, two exact numbers, two inexact numbers, or two characters with the same value may or may not be identical.

    Since constant objects are immutable, i.e., programs should not modify them via vector-set!, set-car!, or any other structure mutation operation, 
    all or portions of different quoted constants or self-evaluating literals may be represented internally by the same object. 
    Thus, eq? may return #t when applied to equal parts of different immutable constants.

    eq? is most often used to compare symbols or to check for pointer equivalence of allocated objects, e.g., pairs, vectors, or record instances.

    (eq? 'a 3) <graphic> #f
    (eq? #t 't) <graphic> #f
    (eq? "abc" 'abc) <graphic> #f
    (eq? "hi" '(hi)) <graphic> #f
    (eq? #f '()) <graphic> #f 

    (eq? 9/2 7/2) <graphic> #f
    (eq? 3.4 53344) <graphic> #f
    (eq? 3 3.0) <graphic> #f
    (eq? 1/3 #i1/3) <graphic> #f 

    (eq? 9/2 9/2) <graphic> unspecified
    (eq? 3.4 (+ 3.0 .4)) <graphic> unspecified
    (let ([x (* 12345678987654321 2)])
      (eq? x x)) <graphic> unspecified 

    (eq? #\a #\b) <graphic> #f
    (eq? #\a #\a) <graphic> unspecified
    (let ([x (string-ref "hi" 0)])
      (eq? x x)) <graphic> unspecified 

    (eq? #t #t) <graphic> #t
    (eq? #f #f) <graphic> #t
    (eq? #t #f) <graphic> #f
    (eq? (null? '()) #t) <graphic> #t
    (eq? (null? '(a)) #f) <graphic> #t 

    (eq? (cdr '(a)) '()) <graphic> #t 

    (eq? 'a 'a) <graphic> #t
    (eq? 'a 'b) <graphic> #f
    (eq? 'a (string->symbol "a")) <graphic> #t 

    (eq? '(a) '(b)) <graphic> #f
    (eq? '(a) '(a)) <graphic> unspecified
    (let ([x '(a . b)]) (eq? x x)) <graphic> #t
    (let ([x (cons 'a 'b)])
      (eq? x x)) <graphic> #t
    (eq? (cons 'a 'b) (cons 'a 'b)) <graphic> #f 

    (eq? "abc" "cba") <graphic> #f
    (eq? "abc" "abc") <graphic> unspecified
    (let ([x "hi"]) (eq? x x)) <graphic> #t
    (let ([x (string #\h #\i)]) (eq? x x)) <graphic> #t
    (eq? (string #\h #\i)
         (string #\h #\i)) <graphic> #f 

    (eq? '#vu8(1) '#vu8(1)) <graphic> unspecified
    (eq? '#vu8(1) '#vu8(2)) <graphic> #f
    (let ([x (make-bytevector 10 0)])
      (eq? x x)) <graphic> #t
    (let ([x (make-bytevector 10 0)])
      (eq? x (make-bytevector 10 0))) <graphic> #f 

    (eq? '#(a) '#(b)) <graphic> #f
    (eq? '#(a) '#(a)) <graphic> unspecified
    (let ([x '#(a)]) (eq? x x)) <graphic> #t
    (let ([x (vector 'a)])
      (eq? x x)) <graphic> #t
    (eq? (vector 'a) (vector 'a)) <graphic> #f 

    (eq? car car) <graphic> #t
    (eq? car cdr) <graphic> #f
    (let ([f (lambda (x) x)])
      (eq? f f)) <graphic> #t
    (let ([f (lambda () (lambda (x) x))])
      (eq? (f) (f))) <graphic> unspecified
    (eq? (lambda (x) x) (lambda (y) y)) <graphic> unspecified 

    (let ([f (lambda (x)
               (lambda ()
                 (set! x (+ x 1))
                 x))])
      (eq? (f 0) (f 0))) <graphic> #f

    procedure: (eqv? obj1 obj2) 
    returns: #t if obj1 and obj2 are equivalent, #f otherwise 
    libraries: (rnrs base), (rnrs)





