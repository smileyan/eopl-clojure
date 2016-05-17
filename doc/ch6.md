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

    eqv? is similar to eq? except eqv? is guaranteed to return #t for two characters 
    that are considered equal by char=? and two numbers that are 
    (a) considered equal by = and (b) cannot be distinguished by any other operation besides eq? and eqv?.
    A consequence of (b) is that (eqv? -0.0 +0.0) is #f even though (= -0.0 +0.0) is #t in systems 
    that distinguish -0.0 and +0.0, such as those based on IEEE floating-point arithmetic. 
    This is because operations such as / can expose the difference:

    (/ 1.0 -0.0) <graphic> -inf.0
    (/ 1.0 +0.0) <graphic> +inf.0

    Similarly, although 3.0 and 3.0+0.0i are considered numerically equal, 
    they are not considered equivalent by eqv? if -0.0 and 0.0 have different representations.

    (= 3.0+0.0i 3.0) <graphic> #t
    (eqv? 3.0+0.0i 3.0) <graphic> #f

    The boolean value returned by eqv? is not specified when the arguments are NaNs.

    (eqv? +nan.0 (/ 0.0 0.0)) <graphic> unspecified

    eqv? is less implementation-dependent but generally more expensive than eq?.

    (eqv? 'a 3) <graphic> #f
    (eqv? #t 't) <graphic> #f
    (eqv? "abc" 'abc) <graphic> #f
    (eqv? "hi" '(hi)) <graphic> #f
    (eqv? #f '()) <graphic> #f 

    (eqv? 9/2 7/2) <graphic> #f
    (eqv? 3.4 53344) <graphic> #f
    (eqv? 3 3.0) <graphic> #f
    (eqv? 1/3 #i1/3) <graphic> #f 

    (eqv? 9/2 9/2) <graphic> #t
    (eqv? 3.4 (+ 3.0 .4)) <graphic> #t
    (let ([x (* 12345678987654321 2)])
      (eqv? x x)) <graphic> #t 

    (eqv? #\a #\b) <graphic> #f
    (eqv? #\a #\a) <graphic> #t
    (let ([x (string-ref "hi" 0)])
      (eqv? x x)) <graphic> #t 

    (eqv? #t #t) <graphic> #t
    (eqv? #f #f) <graphic> #t
    (eqv? #t #f) <graphic> #f
    (eqv? (null? '()) #t) <graphic> #t
    (eqv? (null? '(a)) #f) <graphic> #t 

    (eqv? (cdr '(a)) '()) <graphic> #t 

    (eqv? 'a 'a) <graphic> #t
    (eqv? 'a 'b) <graphic> #f
    (eqv? 'a (string->symbol "a")) <graphic> #t 

    (eqv? '(a) '(b)) <graphic> #f
    (eqv? '(a) '(a)) <graphic> unspecified
    (let ([x '(a . b)]) (eqv? x x)) <graphic> #t
    (let ([x (cons 'a 'b)])
      (eqv? x x)) <graphic> #t
    (eqv? (cons 'a 'b) (cons 'a 'b)) <graphic> #f 

    (eqv? "abc" "cba") <graphic> #f
    (eqv? "abc" "abc") <graphic> unspecified
    (let ([x "hi"]) (eqv? x x)) <graphic> #t
    (let ([x (string #\h #\i)]) (eqv? x x)) <graphic> #t
    (eqv? (string #\h #\i)
          (string #\h #\i)) <graphic> #f 

    (eqv? '#vu8(1) '#vu8(1)) <graphic> unspecified
    (eqv? '#vu8(1) '#vu8(2)) <graphic> #f
    (let ([x (make-bytevector 10 0)])
      (eqv? x x)) <graphic> #t
    (let ([x (make-bytevector 10 0)])
      (eqv? x (make-bytevector 10 0))) <graphic> #f 

    (eqv? '#(a) '#(b)) <graphic> #f
    (eqv? '#(a) '#(a)) <graphic> unspecified
    (let ([x '#(a)]) (eqv? x x)) <graphic> #t
    (let ([x (vector 'a)])
      (eqv? x x)) <graphic> #t
    (eqv? (vector 'a) (vector 'a)) <graphic> #f 

    (eqv? car car) <graphic> #t
    (eqv? car cdr) <graphic> #f
    (let ([f (lambda (x) x)])
      (eqv? f f)) <graphic> #t
    (let ([f (lambda () (lambda (x) x))])
      (eqv? (f) (f))) <graphic> unspecified
    (eqv? (lambda (x) x) (lambda (y) y)) <graphic> unspecified 

    (let ([f (lambda (x)
               (lambda ()
                 (set! x (+ x 1))
                 x))])
    (eqv? (f 0) (f 0))) <graphic> #f

  procedure: (equal? obj1 obj2) 
  returns: #t if obj1 and obj2 have the same structure and contents, #f otherwise 
  libraries: (rnrs base), (rnrs)

    Two objects are equal if they are equivalent according to eqv?, strings that are string=?, bytevectors that are bytevector=?, pairs whose cars and cdrs are equal, or 
    vectors of the same length whose corresponding elements are equal.

    equal? is required to terminate even for cyclic arguments and return #t "if and only if the (possibly infinite) unfoldings of its arguments into regular trees are equal as ordered trees" [24]. 
    In essence, two values are equivalent, in the sense of equal?, if the structure of the two objects cannot be distinguished 
    by any composition of pair and vector accessors along with the eqv?, string=?, and bytevector=? procedures for comparing data at the leaves.

    Implementing equal? efficiently is tricky [1], and even with a good implementation, it is likely to be more expensive than either eqv? or eq?.

    (equal? 'a 3) <graphic> #f
    (equal? #t 't) <graphic> #f
    (equal? "abc" 'abc) <graphic> #f
    (equal? "hi" '(hi)) <graphic> #f
    (equal? #f '()) <graphic> #f 

    (equal? 9/2 7/2) <graphic> #f
    (equal? 3.4 53344) <graphic> #f
    (equal? 3 3.0) <graphic> #f
    (equal? 1/3 #i1/3) <graphic> #f 

    (equal? 9/2 9/2) <graphic> #t
    (equal? 3.4 (+ 3.0 .4)) <graphic> #t
    (let ([x (* 12345678987654321 2)])
      (equal? x x)) <graphic> #t 

    (equal? #\a #\b) <graphic> #f
    (equal? #\a #\a) <graphic> #t
    (let ([x (string-ref "hi" 0)])
      (equal? x x)) <graphic> #t 

    (equal? #t #t) <graphic> #t
    (equal? #f #f) <graphic> #t
    (equal? #t #f) <graphic> #f
    (equal? (null? '()) #t) <graphic> #t
    (equal? (null? '(a)) #f) <graphic> #t 

    (equal? (cdr '(a)) '()) <graphic> #t 

    (equal? 'a 'a) <graphic> #t
    (equal? 'a 'b) <graphic> #f
    (equal? 'a (string->symbol "a")) <graphic> #t 

    (equal? '(a) '(b)) <graphic> #f
    (equal? '(a) '(a)) <graphic> #t
    (let ([x '(a . b)]) (equal? x x)) <graphic> #t
    (let ([x (cons 'a 'b)])
      (equal? x x)) <graphic> #t
    (equal? (cons 'a 'b) (cons 'a 'b)) <graphic> #t 

    (equal? "abc" "cba") <graphic> #f
    (equal? "abc" "abc") <graphic> #t
    (let ([x "hi"]) (equal? x x)) <graphic> #t
    (let ([x (string #\h #\i)]) (equal? x x)) <graphic> #t
    (equal? (string #\h #\i)
            (string #\h #\i)) <graphic> #t 

    (equal? '#vu8(1) '#vu8(1)) <graphic> #t
    (equal? '#vu8(1) '#vu8(2)) <graphic> #f
    (let ([x (make-bytevector 10 0)])
      (equal? x x)) <graphic> #t
    (let ([x (make-bytevector 10 0)])
      (equal? x (make-bytevector 10 0))) <graphic> #t 

    (equal? '#(a) '#(b)) <graphic> #f
    (equal? '#(a) '#(a)) <graphic> #t
    (let ([x '#(a)]) (equal? x x)) <graphic> #t
    (let ([x (vector 'a)])
      (equal? x x)) <graphic> #t
    (equal? (vector 'a) (vector 'a)) <graphic> #t 

    (equal? car car) <graphic> #t
    (equal? car cdr) <graphic> #f
    (let ([f (lambda (x) x)])
      (equal? f f)) <graphic> #t
    (let ([f (lambda () (lambda (x) x))])
      (equal? (f) (f))) <graphic> unspecified
    (equal? (lambda (x) x) (lambda (y) y)) <graphic> unspecified 

    (let ([f (lambda (x)
             (lambda ()
               (set! x (+ x 1))
               x))])
    (equal? (f 0) (f 0))) <graphic> #f 

    (equal?
      (let ([x (cons 'x 'x)])
        (set-car! x x)
        (set-cdr! x x)
        x)
      (let ([x (cons 'x 'x)])
        (set-car! x x)
        (set-cdr! x x)
        (cons x x))) <graphic> #t

    procedure: (boolean? obj) 
    returns: #t if obj is either #t or #f, #f otherwise 
    libraries: (rnrs base), (rnrs)

    boolean? is equivalent to (lambda (x) (or (eq? x #t) (eq? x #f))).

    (boolean? #t) <graphic> #t
    (boolean? #f) <graphic> #t
    (or (boolean? 't) (boolean? '())) <graphic> #f

    procedure: (null? obj) 
    returns: #t if obj is the empty list, #f otherwise 
    libraries: (rnrs base), (rnrs)

    null? is equivalent to (lambda (x) (eq? x '())).

    (null? '()) <graphic> #t
    (null? '(a)) <graphic> #f
    (null? (cdr '(a))) <graphic> #t
    (null? 3) <graphic> #f
    (null? #f) <graphic> #f

    procedure: (pair? obj) 
    returns: #t if obj is a pair, #f otherwise 
    libraries: (rnrs base), (rnrs)

    (pair? '(a b c)) <graphic> #t
    (pair? '(3 . 4)) <graphic> #t
    (pair? '()) <graphic> #f
    (pair? '#(a b)) <graphic> #f
    (pair? 3) <graphic> #f

    procedure: (number? obj) 
    returns: #t if obj is a number object, #f otherwise 
    procedure: (complex? obj) 
    returns: #t if obj is a complex number object, #f otherwise 
    procedure: (real? obj) 
    returns: #t if obj is a real number object, #f otherwise 
    procedure: (rational? obj) 
    returns: #t if obj is a rational number object, #f otherwise 
    procedure: (integer? obj) 
    returns: #t if obj is an integer object, #f otherwise 
    libraries: (rnrs base), (rnrs)

    These predicates form a hierarchy: any integer is rational, any rational is real, any real is complex, and any complex is numeric. 
    Most implementations do not provide internal representations for irrational numbers, so all real numbers are typically rational as well.

    The real?, rational?, and integer? predicates do not recognize as real, rational, or integer complex numbers with inexact zero imaginary parts.

    (integer? 1901) <graphic> #t
    (rational? 1901) <graphic> #t
    (real? 1901) <graphic> #t
    (complex? 1901) <graphic> #t
    (number? 1901) <graphic> #t 

    (integer? -3.0) <graphic> #t
    (rational? -3.0) <graphic> #t
    (real? -3.0) <graphic> #t
    (complex? -3.0) <graphic> #t
    (number? -3.0) <graphic> #t 

    (integer? 7+0i) <graphic> #t
    (rational? 7+0i) <graphic> #t
    (real? 7+0i) <graphic> #t
    (complex? 7+0i) <graphic> #t
    (number? 7+0i) <graphic> #t 

    (integer? -2/3) <graphic> #f
    (rational? -2/3) <graphic> #t
    (real? -2/3) <graphic> #t
    (complex? -2/3) <graphic> #t
    (number? -2/3) <graphic> #t 

    (integer? -2.345) <graphic> #f
    (rational? -2.345) <graphic> #t
    (real? -2.345) <graphic> #t
    (complex? -2.345) <graphic> #t
    (number? -2.345) <graphic> #t 

    (integer? 7.0+0.0i) <graphic> #f
    (rational? 7.0+0.0i) <graphic> #f
    (real? 7.0+0.0i) <graphic> #f
    (complex? 7.0+0.0i) <graphic> #t
    (number? 7.0+0.0i) <graphic> #t 

    (integer? 3.2-2.01i) <graphic> #f
    (rational? 3.2-2.01i) <graphic> #f
    (real? 3.2-2.01i) <graphic> #f
    (complex? 3.2-2.01i) <graphic> #t
    (number? 3.2-2.01i) <graphic> #t 

    (integer? 'a) <graphic> #f
    (rational? '(a b c)) <graphic> #f
    (real? "3") <graphic> #f
    (complex? '#(1 2)) <graphic> #f
    (number? #\a) <graphic> #f

    procedure: (real-valued? obj) 
    returns: #t if obj is a real number, #f otherwise 
    procedure: (rational-valued? obj) 
    returns: #t if obj is a rational number, #f otherwise 
    procedure: (integer-valued? obj) 
    returns: #t if obj is an integer, #f otherwise 
    libraries: (rnrs base), (rnrs)

    These predicates are similar to real?, rational?, and integer?, but treat as real, rational, or integral complex numbers with inexact zero imaginary parts.

    (integer-valued? 1901) <graphic> #t
    (rational-valued? 1901) <graphic> #t
    (real-valued? 1901) <graphic> #t 

    (integer-valued? -3.0) <graphic> #t
    (rational-valued? -3.0) <graphic> #t
    (real-valued? -3.0) <graphic> #t 

    (integer-valued? 7+0i) <graphic> #t
    (rational-valued? 7+0i) <graphic> #t
    (real-valued? 7+0i) <graphic> #t 

    (integer-valued? -2/3) <graphic> #f
    (rational-valued? -2/3) <graphic> #t
    (real-valued? -2/3) <graphic> #t 

    (integer-valued? -2.345) <graphic> #f
    (rational-valued? -2.345) <graphic> #t
    (real-valued? -2.345) <graphic> #t 

    (integer-valued? 7.0+0.0i) <graphic> #t
    (rational-valued? 7.0+0.0i) <graphic> #t
    (real-valued? 7.0+0.0i) <graphic> #t 

    (integer-valued? 3.2-2.01i) <graphic> #f
    (rational-valued? 3.2-2.01i) <graphic> #f
    (real-valued? 3.2-2.01i) <graphic> #f

    As with real?, rational?, and integer?, these predicates return #f for all non-numeric values.

    (integer-valued? 'a) <graphic> #f
    (rational-valued? '(a b c)) <graphic> #f
    (real-valued? "3") <graphic> #f

    procedure: (char? obj) 
    returns: #t if obj is a character, #f otherwise 
    libraries: (rnrs base), (rnrs)

    (char? 'a) <graphic> #f
    (char? 97) <graphic> #f
    (char? #\a) <graphic> #t
    (char? "a") <graphic> #f
    (char? (string-ref (make-string 1) 0)) <graphic> #t

    procedure: (string? obj) 
    returns: #t if obj is a string, #f otherwise 
    libraries: (rnrs base), (rnrs)

    (string? "hi") <graphic> #t
    (string? 'hi) <graphic> #f
    (string? #\h) <graphic> #f

    procedure: (vector? obj) 
    returns: #t if obj is a vector, #f otherwise 
    libraries: (rnrs base), (rnrs)

    (vector? '#()) <graphic> #t
    (vector? '#(a b c)) <graphic> #t
    (vector? (vector 'a 'b 'c)) <graphic> #t
    (vector? '()) <graphic> #f
    (vector? '(a b c)) <graphic> #f
    (vector? "abc") <graphic> #f

    procedure: (symbol? obj) 
    returns: #t if obj is a symbol, #f otherwise 
    libraries: (rnrs base), (rnrs)

    (symbol? 't) <graphic> #t
    (symbol? "t") <graphic> #f
    (symbol? '(t)) <graphic> #f
    (symbol? #\t) <graphic> #f
    (symbol? 3) <graphic> #f
    (symbol? #t) <graphic> #f

    procedure: (procedure? obj) 
    returns: #t if obj is a procedure, #f otherwise 
    libraries: (rnrs base), (rnrs)

    (procedure? car) <graphic> #t
    (procedure? 'car) <graphic> #f
    (procedure? (lambda (x) x)) <graphic> #t
    (procedure? '(lambda (x) x)) <graphic> #f
    (call/cc procedure?) <graphic> #t

    procedure: (bytevector? obj) 
    returns: #t if obj is a bytevector, #f otherwise 
    libraries: (rnrs bytevectors), (rnrs)

    (bytevector? #vu8()) <graphic> #t
    (bytevector? '#()) <graphic> #f
    (bytevector? "abc") <graphic> #f

    procedure: (hashtable? obj) 
    returns: #t if obj is a hashtable, #f otherwise 
    libraries: (rnrs hashtables), (rnrs)

    (hashtable? (make-eq-hashtable)) <graphic> #t
    (hashtable? '(not a hash table)) <graphic> #f

  Section 6.3. Lists and Pairs






