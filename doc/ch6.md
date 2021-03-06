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

    The pair, or cons cell, is the most fundamental of Scheme's structured object types. 
    The most common use for pairs is to build lists, which are ordered sequences of pairs linked one to the next by the cdr field. 
    The elements of the list occupy the car fields of the pairs. The cdr of the last pair in a proper list is the empty list, (); the cdr of the last pair in an improper list can be anything other than ().

    Pairs may be used to construct binary trees. Each pair in the tree structure is an internal node of the binary tree; its car and cdr are the children of the node.

    Proper lists are printed as sequences of objects separated by whitespace and enclosed in parentheses. 
    Matching pairs of brackets ( [ ] ) may be used in place of parentheses. For example, (1 2 3) and (a [nested list]) are proper lists. The empty list is written as ().

    Improper lists and trees require a slightly more complex syntax. 
    A single pair is written as two objects separated by whitespace and a dot, e.g., (a . b). This is referred to as dotted-pair notation. 
    Improper lists and trees are also written in dotted-pair notation; the dot appears wherever necessary, e.g., (1 2 3 . 4) or ((1 . 2) . 3). 
    Proper lists may be written in dotted-pair notation as well. For example, (1 2 3) may be written as (1 . (2 . (3 . ()))).

    It is possible to create a circular list or a cyclic graph by destructively altering the car or cdr field of a pair, using set-car! or set-cdr!. Such lists are not considered proper lists.

    Procedures that accept a list argument are required to detect that the list is improper only to the extent that they actually traverse the list far enough either 
    (a) to attempt to operate on a non-list tail or 
    (b) to loop indefinitely due to a circularity. 
    For example, member need not detect that a list is improper if it actually finds the element being sought, 
    and list-ref need never detect circularities, because its recursion is bounded by the index argument.

    procedure: (cons obj1 obj2) 
    returns: a new pair whose car and cdr are obj1 and obj2 
    libraries: (rnrs base), (rnrs)

    cons is the pair constructor procedure. obj1 becomes the car and obj2 becomes the cdr of the new pair.

    (cons 'a '()) <graphic> (a)
    (cons 'a '(b c)) <graphic> (a b c)
    (cons 3 4) <graphic> (3 . 4)

    procedure: (car pair) 
    returns: the car of pair 
    libraries: (rnrs base), (rnrs)

    The empty list is not a pair, so the argument must not be the empty list.

    (car '(a)) <graphic> a
    (car '(a b c)) <graphic> a
    (car (cons 3 4)) <graphic> 3

    procedure: (cdr pair) 
    returns: the cdr of pair 
    libraries: (rnrs base), (rnrs)

    The empty list is not a pair, so the argument must not be the empty list.

    (cdr '(a)) <graphic> ()
    (cdr '(a b c)) <graphic> (b c)
    (cdr (cons 3 4)) <graphic> 4

    procedure: (set-car! pair obj) 
    returns: unspecified 
    libraries: (rnrs mutable-pairs)

    set-car! changes the car of pair to obj.

    (let ([x (list 'a 'b 'c)])
      (set-car! x 1)
      x) <graphic> (1 b c)

    procedure: (set-cdr! pair obj) 
    returns: unspecified 
    libraries: (rnrs mutable-pairs)

    set-cdr! changes the cdr of pair to obj.

    (let ([x (list 'a 'b 'c)])
      (set-cdr! x 1)
      x) <graphic> (a . 1)

    procedure: (caar pair) 
    procedure: (cadr pair)  <graphic> 
    procedure: (cddddr pair) 
    returns: the caar, cadr, ..., or cddddr of pair 
    libraries: (rnrs base), (rnrs)

    These procedures are defined as the composition of up to four cars and cdrs. The a's and d's between the c and r represent the application of car or cdr in order from right to left. 
    For example, the procedure cadr applied to a pair yields the car of the cdr of the pair and is equivalent to (lambda (x) (car (cdr x))).

    (caar '((a))) <graphic> a
    (cadr '(a b c)) <graphic> b
    (cdddr '(a b c d)) <graphic> (d)
    (cadadr '(a (b c))) <graphic> c

    procedure: (list obj ...) 
    returns: a list of obj ... 
    libraries: (rnrs base), (rnrs)

    list is equivalent to (lambda x x).

    (list) <graphic> ()
    (list 1 2 3) <graphic> (1 2 3)
    (list 3 2 1) <graphic> (3 2 1)

    procedure: (cons* obj ... final-obj) 
    returns: a list of obj ... terminated by final-obj 
    libraries: (rnrs lists), (rnrs)

    If the objects obj ... are omitted, the result is simply final-obj. Otherwise, a list of obj ... is constructed, as with list, except that the final cdr field is final-obj instead of (). 
    If final-obj is not a list, the result is an improper list.

    (cons* '()) <graphic> ()
    (cons* '(a b)) <graphic> (a b)
    (cons* 'a 'b 'c) <graphic> (a b . c)
    (cons* 'a 'b '(c d)) <graphic> (a b c d)

    procedure: (list? obj) 
    returns: #t if obj is a proper list, #f otherwise 
    libraries: (rnrs base), (rnrs)

    list? must return #f for all improper lists, including cyclic lists. A definition of list? is shown on page 67.

    (list? '()) <graphic> #t
    (list? '(a b c)) <graphic> #t
    (list? 'a) <graphic> #f
    (list? '(3 . 4)) <graphic> #f
    (list? 3) <graphic> #f
    (let ([x (list 'a 'b 'c)])
      (set-cdr! (cddr x) x)
      (list? x)) <graphic> #f

    procedure: (length list) 
    returns: the number of elements in list 
    libraries: (rnrs base), (rnrs)

    length may be defined as follows, using an adaptation of the hare and tortoise algorithm used for the definition of list? on page 67.

    (define length
      (lambda (x)
        (define improper-list
          (lambda ()
            (assertion-violation 'length "not a proper list" x))) 

        (let f ([h x] [t x] [n 0])
          (if (pair? h)
              (let ([h (cdr h)])
                (if (pair? h)
                    (if (eq? h t)
                        (improper-list)
                        (f (cdr h) (cdr t) (+ n 2)))
                    (if (null? h)
                        (+ n 1)
                        (improper-list))))
              (if (null? h)
                  n
                  (improper-list)))))) 

    (length '()) <graphic> 0
    (length '(a b c)) <graphic> 3
    (length '(a b . c)) <graphic> exception
    (length
      (let ([ls (list 'a 'b)])
        (set-cdr! (cdr ls) ls) <graphic> exception
        ls))
    (length
      (let ([ls (list 'a 'b)])
        (set-car! (cdr ls) ls) <graphic> 2
        ls))

    procedure: (list-ref list n) 
    returns: the nth element (zero-based) of list 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer less than the length of list. list-ref may be defined without error checks as follows.

    (define list-ref
      (lambda (ls n)
        (if (= n 0)
            (car ls)
            (list-ref (cdr ls) (- n 1))))) 

    (list-ref '(a b c) 0) <graphic> a
    (list-ref '(a b c) 1) <graphic> b
    (list-ref '(a b c) 2) <graphic> c

    procedure: (list-tail list n) 
    returns: the nth tail (zero-based) of list 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer less than or equal to the length of list. The result is not a copy; the tail is eq? to the nth cdr of list (or to list itself, if n is zero).

    list-tail may be defined without error checks as follows.

    (define list-tail
      (lambda (ls n)
        (if (= n 0)
            ls
            (list-tail (cdr ls) (- n 1))))) 

    (list-tail '(a b c) 0) <graphic> (a b c)
    (list-tail '(a b c) 2) <graphic> (c)
    (list-tail '(a b c) 3) <graphic> ()
    (list-tail '(a b c . d) 2) <graphic> (c . d)
    (list-tail '(a b c . d) 3) <graphic> d
    (let ([x (list 1 2 3)])
      (eq? (list-tail x 2)
           (cddr x))) <graphic> #t

    procedure: (append) 
    procedure: (append list ... obj) 
    returns: the concatenation of the input lists 
    libraries: (rnrs base), (rnrs)

    append returns a new list consisting of the elements of the first list followed by the elements of the second list, the elements of the third list, and so on. 
    The new list is made from new pairs for all arguments but the last; the last (which need not be a list) is merely placed at the end of the new structure. 
    append may be defined without error checks as follows.

    (define append
      (lambda args
        (let f ([ls '()] [args args])
          (if (null? args)
              ls
              (let g ([ls ls])
                (if (null? ls)
                    (f (car args) (cdr args))
                    (cons (car ls) (g (cdr ls))))))))) 

    (append '(a b c) '()) <graphic> (a b c)
    (append '() '(a b c)) <graphic> (a b c)
    (append '(a b) '(c d)) <graphic> (a b c d)
    (append '(a b) 'c) <graphic> (a b . c)
    (let ([x (list 'b)])
      (eq? x (cdr (append '(a) x)))) <graphic> #t

    procedure: (reverse list) 
    returns: a new list containing the elements of list in reverse order 
    libraries: (rnrs base), (rnrs)

    reverse may be defined without error checks as follows.

    (define reverse
      (lambda (ls)
        (let rev ([ls ls] [new '()])
          (if (null? ls)
              new
              (rev (cdr ls) (cons (car ls) new)))))) 

    (reverse '()) <graphic> ()
    (reverse '(a b c)) <graphic> (c b a)

    procedure: (memq obj list) 
    procedure: (memv obj list) 
    procedure: (member obj list) 
    returns: the first tail of list whose car is equivalent to obj, or #f 
    libraries: (rnrs lists), (rnrs)

    These procedures traverse the argument list in order, comparing the elements of list against obj. 
    If an object equivalent to obj is found, the tail of the list whose first element is that object is returned. 
    If the list contains more than one object equivalent to obj, the first tail whose first element is equivalent to obj is returned. 
    If no object equivalent to obj is found, #f is returned. The equivalence test for memq is eq?, for memv is eqv?, and for member is equal?.

    These procedures are most often used as predicates, but their names do not end with a question mark because they return a useful true value in place of #t. 
    memq may be defined without error checks as follows.

    (define memq
      (lambda (x ls)
        (cond
          [(null? ls) #f]
          [(eq? (car ls) x) ls]
          [else (memq x (cdr ls))])))

    memv and member may be defined similarly, with eqv? and equal? in place of eq?.

    (memq 'a '(b c a d e)) <graphic> (a d e)
    (memq 'a '(b c d e g)) <graphic> #f
    (memq 'a '(b a c a d a)) <graphic> (a c a d a) 

    (memv 3.4 '(1.2 2.3 3.4 4.5)) <graphic> (3.4 4.5)
    (memv 3.4 '(1.3 2.5 3.7 4.9)) <graphic> #f
    (let ([ls (list 'a 'b 'c)])
      (set-car! (memv 'b ls) 'z)
      ls) <graphic> (a z c) 

    (member '(b) '((a) (b) (c))) <graphic> ((b) (c))
    (member '(d) '((a) (b) (c))) <graphic> #f
    (member "b" '("a" "b" "c")) <graphic> ("b" "c") 

    (let ()
      (define member?
        (lambda (x ls)
          (and (member x ls) #t)))
      (member? '(b) '((a) (b) (c)))) <graphic> #t 

    (define count-occurrences
      (lambda (x ls)
        (cond
          [(memq x ls) =>
           (lambda (ls)
             (+ (count-occurrences x (cdr ls)) 1))]
          [else 0]))) 

    (count-occurrences 'a '(a b c d a)) <graphic> 2

    procedure: (memp procedure list) 
    returns: the first tail of list for whose car procedure returns true, or #f 
    libraries: (rnrs lists), (rnrs)

    procedure should accept one argument and return a single value. It should not modify list.

    (memp odd? '(1 2 3 4)) <graphic> (1 2 3 4)
    (memp even? '(1 2 3 4)) <graphic> (2 3 4)
    (let ([ls (list 1 2 3 4)])
      (eq? (memp odd? ls) ls)) <graphic> #t
    (let ([ls (list 1 2 3 4)])
      (eq? (memp even? ls) (cdr ls))) <graphic> #t
    (memp odd? '(2 4 6 8)) <graphic> #f

    procedure: (remq obj list) 
    procedure: (remv obj list) 
    procedure: (remove obj list) 
    returns: a list containing the elements of list with all occurrences of obj removed 
    libraries: (rnrs lists), (rnrs)

    These procedures traverse the argument list, removing any objects that are equivalent to obj. 
    The elements remaining in the output list are in the same order as they appear in the input list. 
    If a tail of list (including list itself) contains no occurrences of obj, 
    the corresponding tail of the result list may be the same (by eq?) as the tail of the input list.

    The equivalence test for remq is eq?, for remv is eqv?, and for remove is equal?.

    (remq 'a '(a b a c a d)) <graphic> (b c d)
    (remq 'a '(b c d)) <graphic> (b c d) 

    (remv 1/2 '(1.2 1/2 0.5 3/2 4)) <graphic> (1.2 0.5 3/2 4) 

    (remove '(b) '((a) (b) (c))) <graphic> ((a) (c))

    procedure: (remp procedure list) 
    returns: a list of the elements of list for which procedure returns #f 
    libraries: (rnrs lists), (rnrs)

    procedure should accept one argument and return a single value. It should not modify list.

    remp applies procedure to each element of list and returns a list containing only the elements for which procedure returns #f. 
    The elements of the returned list appear in the same order as they appeared in the original list.

    (remp odd? '(1 2 3 4)) <graphic> (2 4)
    (remp
      (lambda (x) (and (> x 0) (< x 10)))
      '(-5 15 3 14 -20 6 0 -9)) <graphic> (-5 15 14 -20 0 -9)

    procedure: (filter procedure list) 
    returns: a list of the elements of list for which procedure returns true 
    libraries: (rnrs lists), (rnrs)

    procedure should accept one argument and return a single value. It should not modify list.

    filter applies procedure to each element of list and returns a new list containing only the elements for which procedure returns true. 
    The elements of the returned list appear in the same order as they appeared in the original list.

    (filter odd? '(1 2 3 4)) <graphic> (1 3)
    (filter
      (lambda (x) (and (> x 0) (< x 10)))
      '(-5 15 3 14 -20 6 0 -9)) <graphic> (3 6)

    procedure: (partition procedure list) 
    returns: see below 
    libraries: (rnrs lists), (rnrs)

    procedure should accept one argument and return a single value. It should not modify list.

    partition applies procedure to each element of list and returns two values: 
    a new list containing only the elements for which procedure returns true, 
    and a new list containing only the elements for which procedure returns #f. 
    The elements of the returned lists appear in the same order as they appeared in the original list.

    (partition odd? '(1 2 3 4)) <graphic> (1 3)
                                          (2 4)
    (partition
      (lambda (x) (and (> x 0) (< x 10)))
      '(-5 15 3 14 -20 6 0 -9)) <graphic> (3 6)
                                          (-5 15 14 -20 0 -9)

    The values returned by partition can be obtained by calling filter and remp separately, 
    but this would require two calls to procedure for each element of list.

    procedure: (find procedure list) 
    returns: the first element of list for which procedure returns true, or #f 
    libraries: (rnrs lists), (rnrs)

    procedure should accept one argument and return a single value. It should not modify list.

    find traverses the argument list in order, applying procedure to each element in turn. 
    If procedure returns a true value for a given element, find returns that element without applying procedure to the remaining elements. 
    If procedure returns #f for each element of list, find returns #f.

    If a program must distinguish between finding #f in the list and finding no element at all, memp should be used instead.

    (find odd? '(1 2 3 4)) <graphic> 1
    (find even? '(1 2 3 4)) <graphic> 2
    (find odd? '(2 4 6 8)) <graphic> #f
    (find not '(1 a #f 55)) <graphic> #f

    procedure: (assq obj alist) 
    procedure: (assv obj alist) 
    procedure: (assoc obj alist) 
    returns: first element of alist whose car is equivalent to obj, or #f 
    libraries: (rnrs lists), (rnrs)

    The argument alist must be an association list. An association list is a proper list whose elements are key-value pairs of the form (key . value). 
    Associations are useful for storing information (values) associated with certain objects (keys).

    These procedures traverse the association list, testing each key for equivalence with obj. 
    If an equivalent key is found, the key-value pair is returned. Otherwise, #f is returned.

    The equivalence test for assq is eq?, for assv is eqv?, and for assoc is equal?. assq may be defined without error checks as follows.

    (define assq
      (lambda (x ls)
        (cond
          [(null? ls) #f]
          [(eq? (caar ls) x) (car ls)]
          [else (assq x (cdr ls))])))

    assv and assoc may be defined similarly, with eqv? and equal? in place of eq?.

    (assq 'b '((a . 1) (b . 2))) <graphic> (b . 2)
    (cdr (assq 'b '((a . 1) (b . 2)))) <graphic> 2
    (assq 'c '((a . 1) (b . 2))) <graphic> #f 

    (assv 2/3 '((1/3 . 1) (2/3 . 2))) <graphic> (2/3 . 2)
    (assv 2/3 '((1/3 . a) (3/4 . b))) <graphic> #f 

    (assoc '(a) '(((a) . a) (-1 . b))) <graphic> ((a) . a)
    (assoc '(a) '(((b) . b) (a . c))) <graphic> #f 

    (let ([alist (list (cons 2 'a) (cons 3 'b))])
      (set-cdr! (assv 3 alist) 'c)
      alist) <graphic> ((2 . a) (3 . c))

    The interpreter given in Section 12.7 represents environments as association lists and uses assq for both variable lookup and assignment.

    procedure: (assp procedure alist) 
    returns: first element of alist for whose car procedure returns true, or #f 
    libraries: (rnrs lists), (rnrs)

    alist must be an association list. An association list is a proper list whose elements are key-value pairs of the form (key . value). 
    procedure should accept one argument and return a single value. It should not modify list.

    (assp odd? '((1 . a) (2 . b))) <graphic> (1 . a)
    (assp even? '((1 . a) (2 . b))) <graphic> (2 . b)
    (let ([ls (list (cons 1 'a) (cons 2 'b))])
      (eq? (assp odd? ls) (car ls))) <graphic> #t
    (let ([ls (list (cons 1 'a) (cons 2 'b))])
      (eq? (assp even? ls) (cadr ls))) <graphic> #t
    (assp odd? '((2 . b))) <graphic> #f

    procedure: (list-sort predicate list) 
    returns: a list containing the elements of list sorted according to predicate 
    libraries: (rnrs sorting), (rnrs)

    predicate should be a procedure that expects two arguments and returns #t if its first argument must precede its second in the sorted list. 
    That is, if predicate is applied to two elements x and y, where x appears after y in the input list, 
    it should return true only if x should appear before y in the output list. If this constraint is met, list-sort performs a stable sort, i.e., two elements are reordered only when necessary according to predicate. Duplicate elements are not removed. This procedure may call predicate up to nlogn times, where n is the length of list.

    (list-sort < '(3 4 2 1 2 5)) <graphic> (1 2 2 3 4 5)
    (list-sort > '(0.5 1/2)) <graphic> (0.5 1/2)
    (list-sort > '(1/2 0.5)) <graphic> (1/2 0.5)
    (list->string
      (list-sort char>?
        (string->list "hello"))) <graphic> "ollhe"

  Section 6.4. Numbers

    Scheme numbers may be classified as integers, rational numbers, real numbers, or complex numbers. 
    This classification is hierarchical, in that all integers are rational, all rational numbers are real, and all real numbers are complex. 
    The predicates integer?, rational?, real?, and complex? described in Section 6.2 are used to determine into which of these classes a number falls.

    A Scheme number may also be classified as exact or inexact, depending upon the quality of operations used to derive the number and the inputs to these operations. 
    The predicates exact? and inexact? may be used to determine the exactness of a number. 
    Most operations on numbers in Scheme are exactness preserving: if given exact operands they return exact values, and if given inexact operands or a combination of exact and inexact operands they return inexact values.

    Exact integer and rational arithmetic is typically supported to arbitrary precision; the size of an integer or of the denominator or numerator of a ratio is limited only by system storage constraints. 
    Although other representations are possible, inexact numbers are typically represented by floating-point numbers supported by the host computer's hardware or by system software. 
    Complex numbers are typically represented as ordered pairs (real-part, imag-part), where real-part and imag-part are exact integers, exact rationals, or floating-point numbers.

    Scheme numbers are written in a straightforward manner not much different from ordinary conventions for writing numbers. 
    An exact integer is normally written as a sequence of numerals preceded by an optional sign. 
    For example, 3, +19, -100000, and 208423089237489374 all represent exact integers.

    An exact rational number is normally written as two sequences of numerals separated by a slash (/) and preceded by an optional sign. 
    For example, 3/4, -6/5, and 1/1208203823 are all exact rational numbers. 
    A ratio is reduced immediately to lowest terms when it is read and may in fact reduce to an exact integer.

    Inexact real numbers are normally written in either floating-point or scientific notation. 
    Floating-point notation consists of a sequence of numerals followed by a decimal point and another sequence of numerals, all preceded by an optional sign. 
    Scientific notation consists of an optional sign, a sequence of numerals, an optional decimal point followed by a second string of numerals, and an exponent; 
    an exponent is written as the letter e followed by an optional sign and a sequence of numerals. 
    For example, 1.0 and -200.0 are valid inexact integers, and 1.5, 0.034, -10e-10 and 1.5e-5 are valid inexact rational numbers. 
    The exponent is the power of ten by which the number preceding the exponent should be scaled, so that 2e3 is equivalent to 2000.0.

    A mantissa width |w may appear as the suffix of a real number or the real components of a complex number written in floating-point or scientific notation. 
    The mantissa width m represents the number of significant bits in the representation of the number. 
    The mantissa width defaults to 53, the number of significant bits in a normalized IEEE double floating-point number, or more. 
    For denormalized IEEE double floating-point numbers, the mantissa width is less than 53. 
    If an implementation cannot represent a number with the mantissa width specified, it uses a representation with at least as many significant bits as requested if possible, otherwise it uses its representation with the largest mantissa width.

    Exact and inexact real numbers are written as exact or inexact integers or rational numbers; no provision is made in the syntax of Scheme numbers for nonrational real numbers, i.e., irrational numbers.

    Complex numbers may be written in either rectangular or polar form. 
    In rectangular form, a complex number is written as x+yi or x-yi, where x is an integer, rational, or real number and y is an unsigned integer, rational, or real number. 
    The real part, x, may be omitted, in which case it is assumed to be zero. 
    For example, 3+4i, 3.2-3/4i, +i, and -3e-5i are complex numbers written in rectangular form. 
    In polar form, a complex number is written as x@y, where x and y are integer, rational, or real numbers. For example, 1.1@1.764 and -1@-1/2 are complex numbers written in polar form.

    ...

  Section 6.5. Fixnums

    ...

  Section 6.6. Flonums

    ...

  Section 6.7. Characters

    Characters are atomic objects representing letters, digits, special symbols such as $ or -, and certain nongraphic control characters such as space and newline. 
    Characters are written with a #\ prefix. For most characters, the prefix is followed by the character itself. 
    The written character representation of the letter A, for example, is #\A. The characters newline, space, and tab may be written in this manner as well, 
    but they can be written more clearly as #\newline, #\space, and #\tab. Other character names are supported as well, as defined by the grammar for character objects on page 457. 
    Any Unicode character may be written with the syntax #\xn, where n consists of one or more hexadecimal digits and represents a valid Unicode scalar value.

    This section describes the operations that deal primarily with characters. See also the following section on strings and Chapter 7 on input and output for other operations relating to characters.

    procedure: (char=? char1 char2 char3 ...) 
    procedure: (char<? char1 char2 char3 ...) 
    procedure: (char>? char1 char2 char3 ...) 
    procedure: (char<=? char1 char2 char3 ...) 
    procedure: (char>=? char1 char2 char3 ...) 
    returns: #t if the relation holds, #f otherwise 
    libraries: (rnrs base), (rnrs)

    These predicates behave in a similar manner to the numeric predicates =, <, >, <=, and >=. 
    For example, char=? returns #t when its arguments are equivalent characters, and char<? returns #t when its arguments are monotonically increasing character (Unicode scalar) values.

    (char>? #\a #\b) <graphic> #f
    (char<? #\a #\b) <graphic> #t
    (char<? #\a #\b #\c) <graphic> #t
    (let ([c #\r])
      (char<=? #\a c #\z)) <graphic> #t
    (char<=? #\Z #\W) <graphic> #f
    (char=? #\+ #\+) <graphic> #t

    procedure: (char-ci=? char1 char2 char3 ...) 
    procedure: (char-ci<? char1 char2 char3 ...) 
    procedure: (char-ci>? char1 char2 char3 ...) 
    procedure: (char-ci<=? char1 char2 char3 ...) 
    procedure: (char-ci>=? char1 char2 char3 ...) 
    returns: #t if the relation holds, #f otherwise 
    libraries: (rnrs unicode), (rnrs)

    These predicates are identical to the predicates char=?, char<?, char>?, char<=?, and char>=? except that they are case-insensitive, i.e., 
    compare the case-folded versions of their arguments. For example, char=? considers #\a and #\A to be distinct values; char-ci=? does not.

    (char-ci<? #\a #\B) <graphic> #t
    (char-ci=? #\W #\w) <graphic> #t
    (char-ci=? #\= #\+) <graphic> #f
    (let ([c #\R])
      (list (char<=? #\a c #\z)
            (char-ci<=? #\a c #\z))) <graphic> (#f #t)

    procedure: (char-alphabetic? char) 
    returns: #t if char is a letter, #f otherwise 
    procedure: (char-numeric? char) 
    returns: #t if char is a digit, #f otherwise 
    procedure: (char-whitespace? char) 
    returns: #t if char is whitespace, #f otherwise 
    libraries: (rnrs unicode), (rnrs)

    A character is alphabetic if it has the Unicode "Alphabetic" property, numeric if it has the Unicode "Numeric" property, and whitespace if has the Unicode "White_Space" property.

    (char-alphabetic? #\a) <graphic> #t
    (char-alphabetic? #\T) <graphic> #t
    (char-alphabetic? #\8) <graphic> #f
    (char-alphabetic? #\$) <graphic> #f 

    (char-numeric? #\7) <graphic> #t
    (char-numeric? #\2) <graphic> #t
    (char-numeric? #\X) <graphic> #f
    (char-numeric? #\space) <graphic> #f 

    (char-whitespace? #\space) <graphic> #t
    (char-whitespace? #\newline) <graphic> #t
    (char-whitespace? #\Z) <graphic> #f

    procedure: (char-lower-case? char) 
    returns: #t if char is lower case, #f otherwise 
    procedure: (char-upper-case? char) 
    returns: #t if char is upper case, #f otherwise 
    procedure: (char-title-case? char) 
    returns: #t if char is title case, #f otherwise 
    libraries: (rnrs unicode), (rnrs)

    A character is upper-case if it has the Unicode "Uppercase" property, lower-case if it has the "Lowercase" property, and title-case if it is in the Lt general category.

    (char-lower-case? #\r) <graphic> #t
    (char-lower-case? #\R) <graphic> #f 

    (char-upper-case? #\r) <graphic> #f
    (char-upper-case? #\R) <graphic> #t 

    (char-title-case? #\I) <graphic> #f
    (char-title-case? #\x01C5) <graphic> #t

    procedure: (char-general-category char) 
    returns: a symbol representing the Unicode general category of char 
    libraries: (rnrs unicode), (rnrs)

    The return value is one of the symbols Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Ps, Pe, Pi, Pf, Pd, Pc, Po, Sc, Sm, Sk, So, Zs, Zp, Zl, Cc, Cf, Cs, Co, or Cn.

    (char-general-category #\a) <graphic> Ll
    (char-general-category #\space) <graphic> Zs
    (char-general-category #\x10FFFF) <graphic> Cn  

    procedure: (char-upcase char) 
    returns: the upper-case character counterpart of char 
    libraries: (rnrs unicode), (rnrs)

    If char is a lower- or title-case character and has a single upper-case counterpart, char-upcase returns the upper-case counterpart. Otherwise char-upcase returns char.

    (char-upcase #\g) <graphic> #\G
    (char-upcase #\G) <graphic> #\G
    (char-upcase #\7) <graphic> #\7
    (char-upcase #\<graphic>) <graphic> #\<graphic>

    procedure: (char-downcase char) 
    returns: the lower-case character equivalent of char 
    libraries: (rnrs unicode), (rnrs)

    If char is an upper- or title-case character and has a single lower-case counterpart, char-downcase returns the lower-case counterpart. Otherwise char-downcase returns char.

    (char-downcase #\g) <graphic> #\g
    (char-downcase #\G) <graphic> #\g
    (char-downcase #\7) <graphic> #\7
    (char-downcase #\<graphic>) <graphic> #\<graphic>

    procedure: (char-titlecase char) 
    returns: the title-case character equivalent of char 
    libraries: (rnrs unicode), (rnrs)

    If char is an upper- or lower-case character and has a single title-case counterpart, char-titlecase returns the title-case counterpart. 
    Otherwise, if it is not a title-case character, has no single title-case counterpart, but does have a single upper-case counterpart, char-titlecase returns the upper-case counterpart. Otherwise char-titlecase returns char.

    (char-titlecase #\g) <graphic> #\G
    (char-titlecase #\G) <graphic> #\G
    (char-titlecase #\7) <graphic> #\7
    (char-titlecase #\<graphic>) <graphic> #\<graphic>

    procedure: (char-foldcase char) 
    returns: the case-folded character equivalent of char 
    libraries: (rnrs unicode), (rnrs)

    If char has a case-folded counterpart, char-foldcase returns the case-folded counterpart. Otherwise, char-foldcase returns char. For most characters, (char-foldcase char) is equivalent to (char-downcase (char-upcase char)), but for Turkic İ and ı, char-foldcase acts as the identity.

    (char-foldcase #\g) <graphic> #\g
    (char-foldcase #\G) <graphic> #\g
    (char-foldcase #\7) <graphic> #\7
    (char-foldcase #\<graphic>) <graphic> #\<graphic>

    procedure: (char->integer char) 
    returns: the Unicode scalar value of char as an exact integer 
    libraries: (rnrs base), (rnrs)

    (char->integer #\newline) <graphic> 10
    (char->integer #\space) <graphic> 32
    (- (char->integer #\Z) (char->integer #\A)) <graphic> 25

    procedure: (integer->char n) 
    returns: the character corresponding to the Unicode scalar value n 
    libraries: (rnrs base), (rnrs)

    n must be an exact integer and a valid Unicode scalar value, i.e., <graphic> or <graphic>.

    (integer->char 48) <graphic> #\0
    (integer->char #x3BB) <graphic> #\<graphic>

  Section 6.8. Strings

    Strings are sequences of characters and are often used as messages, character buffers, or containers for blocks of text. 
    Scheme provides operations for creating strings, extracting characters from strings, obtaining substrings, concatenating strings, and altering the contents of strings.

    A string is written as a sequence of characters enclosed in double quotes, e.g., "hi there". 
    A double quote may be introduced into a string by preceding it by a backward slash, e.g., "two \"quotes\" within". 
    A backward slash may also be included by preceding it with a backward slash, e.g., "a \\slash". 
    Various special characters can be inserted with other two-character sequences, e.g., \n for newline, \r for carriage return, and \t for tab. 
    Any Unicode character may be inserted with the syntax #\xn;, where n consists of one or more hexadecimal digits and represents a valid Unicode scalar value. 
    A grammar defining the precise syntax of strings is given on page 458.

    Strings are indexed by exact nonnegative integers, and the index of the first element of any string is 0. 
    The highest valid index for a given string is one less than its length.

    procedure: (string=? string1 string2 string3 ...) 
    procedure: (string<? string1 string2 string3 ...) 
    procedure: (string>? string1 string2 string3 ...) 
    procedure: (string<=? string1 string2 string3 ...) 
    procedure: (string>=? string1 string2 string3 ...) 
    returns: #t if the relation holds, #f otherwise 
    libraries: (rnrs base), (rnrs)

    As with =, <, >, <=, and >=, these predicates express relationships among all of the arguments. For example, string>? determines if the lexicographic ordering of its arguments is monotonically decreasing.

    The comparisons are based on the character predicates char=? and char<?. 
    Two strings are lexicographically equivalent if they are the same length and consist of the same sequence of characters according to char=?. 
    If two strings differ only in length, the shorter string is considered to be lexicographically less than the longer string. 
    Otherwise, the first character position at which the strings differ (by char=?) determines which string is lexicographically less than the other, according to char<?.

    Two-argument string=? may be defined without error checks as follows.

    (define string=?
      (lambda (s1 s2)
        (let ([n (string-length s1)])
          (and (= (string-length s2) n)
               (let loop ([i 0])
                 (or (= i n)
                     (and (char=? (string-ref s1 i) (string-ref s2 i))
                          (loop (+ i 1)))))))))

    Two-argument string<? may be defined without error checks as follows.

    (define string<?
      (lambda (s1 s2)
        (let ([n1 (string-length s1)] [n2 (string-length s2)])
          (let loop ([i 0])
            (and (not (= i n2))
                 (or (= i n1)
                     (let ([c1 (string-ref s1 i)] [c2 (string-ref s2 i)])
                       (or (char<? c1 c2)
                           (and (char=? c1 c2)
                                (loop (+ i 1)))))))))))

    These definitions may be extended straightforwardly to support three or more arguments. string<=?, string>?, and string>=? may be defined similarly.

    (string=? "mom" "mom") <graphic> #t
    (string<? "mom" "mommy") <graphic> #t
    (string>? "Dad" "Dad") <graphic> #f
    (string=? "Mom and Dad" "mom and dad") <graphic> #f
    (string<? "a" "b" "c") <graphic> #t

    procedure: (string-ci=? string1 string2 string3 ...) 
    procedure: (string-ci<? string1 string2 string3 ...) 
    procedure: (string-ci>? string1 string2 string3 ...) 
    procedure: (string-ci<=? string1 string2 string3 ...) 
    procedure: (string-ci>=? string1 string2 string3 ...) 
    returns: #t if the relation holds, #f otherwise 
    libraries: (rnrs unicode), (rnrs)

    These predicates are identical to string=?, string<?, string>?, string<=?, and string>=? except that they are case-insensitive, i.e., compare the case-folded versions of their arguments.

    (string-ci=? "Mom and Dad" "mom and dad") <graphic> #t
    (string-ci<=? "say what" "Say What!?") <graphic> #t
    (string-ci>? "N" "m" "L" "k") <graphic> #t
    (string-ci=? "Stra\sse" "Strasse") <graphic> #t

    procedure: (string char ...) 
    returns: a string containing the characters char ... 
    libraries: (rnrs base), (rnrs)

    (string) <graphic> ""
    (string #\a #\b #\c) <graphic> "abc"
    (string #\H #\E #\Y #\!) <graphic> "HEY!"

    procedure: (make-string n) 
    procedure: (make-string n char) 
    returns: a string of length n 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer. If char is supplied, the string is filled with n occurrences of char, otherwise the characters contained in the string are unspecified.

    (make-string 0) <graphic> ""
    (make-string 0 #\x) <graphic> ""
    (make-string 5 #\x) <graphic> "xxxxx"

    procedure: (string-length string) 
    returns: the number of characters in string 
    libraries: (rnrs base), (rnrs)

    The length of a string is always an exact nonnegative integer.

    (string-length "abc") <graphic> 3
    (string-length "") <graphic> 0
    (string-length "hi there") <graphic> 8
    (string-length (make-string 1000000)) <graphic> 1000000

    procedure: (string-ref string n) 
    returns: the nth character (zero-based) of string 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer less than the length of string.

    (string-ref "hi there" 0) <graphic> #\h
    (string-ref "hi there" 5) <graphic> #\e

    procedure: (string-set! string n char) 
    returns: unspecified 
    libraries: (rnrs mutable-strings)

    n must be an exact nonnegative integer less than the length of string. string-set! changes the nth element of string to char.

    (let ([str (string-copy "hi three")])
      (string-set! str 5 #\e)
      (string-set! str 6 #\r)
      str) <graphic> "hi there"

    procedure: (string-copy string) 
    returns: a new copy of string 
    libraries: (rnrs base), (rnrs)

    This procedure creates a new string with the same length and contents as string.

    (string-copy "abc") <graphic> "abc" 

    (let ([str "abc"])
      (eq? str (string-copy str))) <graphic> #f

    procedure: (string-append string ...) 
    returns: a new string formed by concatenating the strings string ... 
    libraries: (rnrs base), (rnrs)

    (string-append) <graphic> ""
    (string-append "abc" "def") <graphic> "abcdef"
    (string-append "Hey " "you " "there!") <graphic> "Hey you there!"

    The following implementation of string-append recurs down the list of strings to compute the total length, then allocates the new string, then fills it up as it unwinds the recursion.

    (define string-append
      (lambda args
        (let f ([ls args] [n 0])
          (if (null? ls)
              (make-string n)
              (let* ([s1 (car ls)]
                    [m (string-length s1)]
                    [s2 (f (cdr ls) (+ n m))])
                (do ([i 0 (+ i 1)] [j n (+ j 1)])
                    ((= i m) s2)
                  (string-set! s2 j (string-ref s1 i))))))))

    procedure: (substring string start end) 
    returns: a copy of string from start (inclusive) to end (exclusive) 
    libraries: (rnrs base), (rnrs)

    start and end must be exact nonnegative integers; start must be less than the length of string, 
    while end may be less than or equal to the length of string. 
    If end ≤ start, a string of length zero is returned. substring may be defined without error checks as follows.

    (define substring
      (lambda (s1 m n)
        (let ([s2 (make-string (- n m))])
          (do ([j 0 (+ j 1)] [i m (+ i 1)])
              ((= i n) s2)
            (string-set! s2 j (string-ref s1 i)))))) 

    (substring "hi there" 0 1) <graphic> "h"
    (substring "hi there" 3 6) <graphic> "the"
    (substring "hi there" 5 5) <graphic> "" 

    (let ([str "hi there"])
      (let ([end (string-length str)])
        (substring str 0 end))) <graphic> "hi there"

    procedure: (string-fill! string char) 
    returns: unspecified 
    libraries: (rnrs mutable-strings)

    string-fill! sets every character in string to char.

    (let ([str (string-copy "sleepy")])
      (string-fill! str #\Z)
      str) <graphic> "ZZZZZZ"

    string-fill! might be defined as follows:

    (define string-fill!
      (lambda (s c)
        (let ([n (string-length s)])
          (do ([i 0 (+ i 1)])
              ((= i n))
              (string-set! s i c)))))

    An alternative definition is given on page 276.

    procedure: (string-upcase string) 
    returns: the upper-case equivalent of string 
    procedure: (string-downcase string) 
    returns: the lower-case equivalent of string 
    procedure: (string-foldcase string) 
    returns: the case-folded equivalent of string 
    procedure: (string-titlecase string) 
    returns: the title-case equivalent of string 
    libraries: (rnrs unicode), (rnrs)

    These procedures implement Unicode's locale-independent case mappings from scalar-value sequences to scalar-value sequences. 
    These mappings do not always map single characters to single characters, so the length of the result string may differ from the length of string. 
    If the result string is the same as string (by string=?), string or a copy of string may be returned. 
    Otherwise, the result string is newly allocated. string-foldcase does not use the special mappings for Turkic languages.

    string-titlecase converts the first cased character of each word in string to its title-case counterpart and 
    converts each other character to its lower-case counterpart. Word breaks are recognized as specified in Unicode Standard Annex #29 [8].

    (string-upcase "Hi") <graphic> "HI"
    (string-downcase "Hi") <graphic> "hi"
    (string-foldcase "Hi") <graphic> "hi" 

    (string-upcase "Straße") <graphic> "STRASSE"
    (string-downcase "Straße") <graphic> "straße"
    (string-foldcase "Straße") <graphic> "strasse"
    (string-downcase "STRASSE")  <graphic> "strasse" 

    (string-downcase "<graphic>") <graphic> "<graphic>" 

    (string-titlecase "kNock KNoCK") <graphic> "Knock Knock"
    (string-titlecase "who's there?") <graphic> "Who's There?"
    (string-titlecase "r6rs") <graphic> "R6rs"
    (string-titlecase "R6RS") <graphic> "R6rs"

    procedure: (string-normalize-nfd string) 
    returns: the Unicode normalized form D of string 
    procedure: (string-normalize-nfkd string) 
    returns: the Unicode normalized form KD of string 
    procedure: (string-normalize-nfc string) 
    returns: the Unicode normalized form C of string 
    procedure: (string-normalize-nfkc string) 
    returns: the Unicode normalized form KC of string 
    libraries: (rnrs unicode), (rnrs)

    If the result string is the same as string (by string=?), string or a copy of string may be returned. 
    Otherwise, the result string is newly allocated.

    (string-normalize-nfd "\xE9;") <graphic> "e\x301;"
    (string-normalize-nfc "\xE9;") <graphic> "\xE9;"
    (string-normalize-nfd "\x65;\x301;") <graphic> "e\x301;"
    (string-normalize-nfc "\x65;\x301;") <graphic> "\xE9;"

    procedure: (string->list string) 
    returns: a list of the characters in string 
    libraries: (rnrs base), (rnrs)

    string->list allows a string to be converted into a list, so that Scheme's list-processing operations may be applied to the processing of strings. 
    string->list may be defined without error checks as follows.

    (define string->list
      (lambda (s)
        (do ([i (- (string-length s) 1) (- i 1)]
            [ls '() (cons (string-ref s i) ls)])
            ((< i 0) ls)))) 

    (string->list "") <graphic> ()
    (string->list "abc") <graphic> (#\a #\b #\c)
    (apply char<? (string->list "abc")) <graphic> #t
    (map char-upcase (string->list "abc")) <graphic> (#\A #\B #\C)

    procedure: (list->string list) 
    returns: a string of the characters in list 
    libraries: (rnrs base), (rnrs)

    list must consist entirely of characters.

    list->string is the functional inverse of string->list. 
    A program might use both procedures together, first converting a string into a list, 
    then operating on this list to produce a new list, 
    and finally converting the new list back into a string.

    list->string may be defined without error checks as follows.

    (define list->string
      (lambda (ls)
        (let ([s (make-string (length ls))])
          (do ([ls ls (cdr ls)] [i 0 (+ i 1)])
              ((null? ls) s)
            (string-set! s i (car ls)))))) 

    (list->string '()) <graphic> ""
    (list->string '(#\a #\b #\c)) <graphic> "abc"
    (list->string
      (map char-upcase
          (string->list "abc"))) <graphic> "ABC"

  Section 6.9. Vectors

    Vectors are more convenient and efficient than lists for some applications. 
    Whereas accessing an arbitrary element in a list requires a linear traversal of the list up to the selected element, 
    arbitrary vector elements are accessed in constant time. The length of a vector is the number of elements it contains. 
    Vectors are indexed by exact nonnegative integers, and the index of the first element of any vector is 0. 
    The highest valid index for a given vector is one less than its length.

    As with lists, the elements of a vector can be of any type, and a single vector can hold more than one type of object.

    A vector is written as a sequence of objects separated by whitespace, preceded by the prefix #( and followed by ). 
    For example, a vector consisting of the elements a, b, and c would be written #(a b c).

    procedure: (vector obj ...) 
    returns: a vector of the objects obj ... 
    libraries: (rnrs base), (rnrs)

    (vector) <graphic> #()
    (vector 'a 'b 'c) <graphic> #(a b c)

    procedure: (make-vector n) 
    procedure: (make-vector n obj) 
    returns: a vector of length n 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer. If obj is supplied, each element of the vector is filled with obj; 
    otherwise, the elements are unspecified.

    (make-vector 0) <graphic> #()
    (make-vector 0 '#(a)) <graphic> #()
    (make-vector 5 '#(a)) <graphic> #(#(a) #(a) #(a) #(a) #(a))

    procedure: (vector-length vector) 
    returns: the number of elements in vector 
    libraries: (rnrs base), (rnrs)

    The length of a vector is always an exact nonnegative integer.

    (vector-length '#()) <graphic> 0
    (vector-length '#(a b c)) <graphic> 3
    (vector-length (vector 1 '(2) 3 '#(4 5))) <graphic> 4
    (vector-length (make-vector 300)) <graphic> 300

    procedure: (vector-ref vector n) 
    returns: the nth element (zero-based) of vector 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer less than the length of vector.

    (vector-ref '#(a b c) 0) <graphic> a
    (vector-ref '#(a b c) 1) <graphic> b
    (vector-ref '#(x y z w) 3) <graphic> w

    procedure: (vector-set! vector n obj) 
    returns: unspecified 
    libraries: (rnrs base), (rnrs)

    n must be an exact nonnegative integer less than the length of vector. vector-set! changes the nth element of vector to obj.

    (let ([v (vector 'a 'b 'c 'd 'e)])
      (vector-set! v 2 'x)
      v) <graphic> #(a b x d e)

    procedure: (vector-fill! vector obj) 
    returns: unspecified 
    libraries: (rnrs base), (rnrs)

    vector-fill! replaces each element of vector with obj. It may be defined without error checks as follows.

    (define vector-fill!
      (lambda (v x)
        (let ([n (vector-length v)])
          (do ([i 0 (+ i 1)])
              ((= i n))
            (vector-set! v i x))))) 

    (let ([v (vector 1 2 3)])
      (vector-fill! v 0)
      v) <graphic> #(0 0 0)

    procedure: (vector->list vector) 
    returns: a list of the elements of vector 
    libraries: (rnrs base), (rnrs)

    vector->list provides a convenient method for applying list-processing operations to vectors. It may be defined without error checks as follows.

    (define vector->list
      (lambda (s)
        (do ([i (- (vector-length s) 1) (- i 1)]
            [ls '() (cons (vector-ref s i) ls)])
            ((< i 0) ls)))) 

    (vector->list (vector)) <graphic> ()
    (vector->list '#(a b c)) <graphic> (a b c) 

    (let ((v '#(1 2 3 4 5)))
      (apply * (vector->list v))) <graphic> 120

    procedure: (list->vector list) 
    returns: a vector of the elements of list 
    libraries: (rnrs base), (rnrs)

    list->vector is the functional inverse of vector->list. The two procedures are often used in combination to take advantage of a list-processing operation. 
    A vector may be converted to a list with vector->list, this list processed in some manner to produce a new list, and the new list converted back into a vector with list->vector.

    list->vector may be defined without error checks as follows.

    (define list->vector
      (lambda (ls)
        (let ([s (make-vector (length ls))])
          (do ([ls ls (cdr ls)] [i 0 (+ i 1)])
              ((null? ls) s)
            (vector-set! s i (car ls)))))) 

    (list->vector '()) <graphic> #()
    (list->vector '(a b c)) <graphic> #(a b c) 

    (let ([v '#(1 2 3 4 5)])
      (let ([ls (vector->list v)])
        (list->vector (map * ls ls)))) <graphic> #(1 4 9 16 25)

    procedure: (vector-sort predicate vector) 
    returns: a vector containing the elements of vector, sorted according to predicate 
    procedure: (vector-sort! predicate vector) 
    returns: unspecified 
    libraries: (rnrs sorting), (rnrs)

    predicate should be a procedure that expects two arguments and returns #t if its first argument must precede its second in the sorted vector. 
    That is, if predicate is applied to two elements x and y, where x appears after y in the input vector, 
    the predicate should return true only if x should appear before y in the output vector. 
    If this constraint is met, vector-sort performs a stable sort, i.e., two elements are reordered only when necessary according to predicate. 
    vector-sort! performs the sort destructively and does not necessarily perform a stable sort.
    Duplicate elements are not removed. predicate should not have any side effects.

    vector-sort may call predicate up to nlogn times, where n is the length of vector, 
    while vector-sort! may call the predicate up to n2 times. 
    The looser bound for vector-sort! allows an implementation to use a quicksort algorithm, 
    which may be faster in some cases than algorithms that have the tighter nlogn bound.

    (vector-sort < '#(3 4 2 1 2 5)) <graphic> #(1 2 2 3 4 5)
    (vector-sort > '#(0.5 1/2)) <graphic> #(0.5 1/2)
    (vector-sort > '#(1/2 0.5)) <graphic> #(1/2 0.5) 

    (let ([v (vector 3 4 2 1 2 5)])
      (vector-sort! < v)
      v) <graphic> #(1 2 2 3 4 5)

  Section 6.10. Bytevectors

    Bytevectors are vectors of raw binary data. 
    Although nominally organized as a sequence of exact unsigned 8-bit integers, a bytevector can be interpreted as a sequence of exact signed 8-bit integers, 
    exact signed or unsigned 16-bit, 32-bit, 64-bit, or arbitrary-precision integers, IEEE single or double floating-point numbers, or arbitrary combinations of the above.

    The length of a bytevector is the number of 8-bit bytes it stores, and indices into a bytevector are always given as byte offsets. 
    Any data element may be aligned at any byte offset, regardless of the underlying hardware's alignment requirements, and may be represented using a specified endianness (see below) that differs from that prescribed by the hardware. 
    Special, typically more efficient operators are provided for 16-, 32-, and 64-bit integers and single and double floats that are in their native format, 
    i.e,. with the endianness of the underlying hardware and stored at an index that is a multiple of the size in bytes of the integer or float.

    The endianness of a multi-byte data value determines how it is laid out in memory. 
    In big-endian format, the value is laid out with the more significant bytes at lower indices, while in little-endian format, the value is laid out with the more significant bytes at higher indices. 
    When a bytevector procedure accepts an endianness argument, the argument may be the symbol big, representing the big-endian format, or the symbol little, representing the little-endian format. 
    Implementations may extend these procedures to accept other endianness symbols. The native endianness of the implementation may be obtained via the procedure native-endianness.

    Bytevectors are written with the #vu8( prefix in place of the #( prefix for vectors, e.g., #vu8(1 2 3). 
    The elements of a bytevector specified in this manner are always given as 8-bit unsigned exact integers, i.e., integers from 0 to 255 inclusive, written using any valid syntax for such numbers. 
    Like strings, bytevectors are self-evaluating, so they need not be quoted.

    '#vu8(1 2 3) <graphic> #vu8(1 2 3)
    #vu8(1 2 3) <graphic> #vu8(1 2 3)
    #vu8(#x3f #x7f #xbf #xff) <graphic> #vu8(63 127 191 255)

    syntax: (endianness symbol) 
    returns: symbol 
    libraries: (rnrs bytevectors), (rnrs)

    symbol must be the symbol little, the symbol big, or some other symbol recognized by the implementation as an endianness symbol. 
    It is a syntax violation if symbol is not a symbol or if it is not recognized by the implementation as an endianness symbol.

    (endianness little) <graphic> little
    (endianness big) <graphic> big
    (endianness "spam") <graphic> exception

    procedure: (native-endianness) 
    returns: a symbol naming the implementation's native endianness 
    libraries: (rnrs bytevectors), (rnrs)

    The return value is the symbol little, the symbol big, or some other endianness symbol recognized by the implementation. It typically reflects the endianness of the underlying hardware.

    (symbol? (native-endianness)) <graphic> #t

    procedure: (make-bytevector n) 
    procedure: (make-bytevector n fill) 
    returns: a new bytevector of length n 
    libraries: (rnrs bytevectors), (rnrs)

    If fill is supplied, each element of the bytevector is initialized to fill; otherwise, the elements are unspecified. 
    The fill value must be a signed or unsigned 8-bit value, i.e., a value in the range -128 to 255 inclusive. A negative fill value is treated as its two's complement equivalent.

    (make-bytevector 0) <graphic> #vu8()
    (make-bytevector 0 7) <graphic> #vu8()
    (make-bytevector 5 7) <graphic> #vu8(7 7 7 7 7)
    (make-bytevector 5 -7) <graphic> #vu8(249 249 249 249 249)

    ...
    ...

  Section 6.11. Symbols

    Symbols are used for a variety of purposes as symbolic names in Scheme programs. 
    Strings could be used for most of the same purposes, but an important characteristic of symbols makes comparisons between symbols much more efficient. 
    This characteristic is that two symbols with the same name are identical in the sense of eq?. 
    The reason is that the Scheme reader (invoked by get-datum and read) and the procedure string->symbol catalog symbols in an internal symbol table and always return the same symbol whenever the same name is encountered. 
    Thus, no character-by-character comparison is needed, as would be needed to compare two strings.

    The property that two symbols may be compared quickly for equivalence makes them ideally suited for use as identifiers in the representation of programs, allowing fast comparison of identifiers. 
    This property also makes symbols useful for a variety of other purposes. 
    For example, symbols might be used as messages passed between procedures, labels for list-structured records, or names for objects stored in an association list (see assq in Section 6.3).

    Symbols are written without double quotes or other bracketing characters. 
    Parentheses, double quotes, spaces, and most other characters with a special meaning to the Scheme reader are not allowed within the printed representation of a symbol. 
    These and any other Unicode character may appear anywhere within the printed representation of a symbol with the syntax #\xn;, where n consists of one or more hexadecimal digits and represents a valid Unicode scalar value.

    The grammar for symbols on page 458 gives a precise definition of the syntax of symbols.

    procedure: (symbol=? symbol1 symbol2) 
    returns: #t if the two symbols are the same, #f otherwise 
    libraries: (rnrs base), (rnrs)

    Symbols can also be compared with eq?, which is typically more efficient than symbol=?.

    (symbol=? 'a 'a) <graphic> #t
    (symbol=? 'a (string->symbol "a")) <graphic> #t
    (symbol=? 'a 'b) <graphic> #f

    procedure: (string->symbol string) 
    returns: a symbol whose name is string 
    libraries: (rnrs base), (rnrs)

    string->symbol records all symbols it creates in an internal table that it shares with the system reader. If a symbol whose name is equivalent to string (according to the predicate string=?) already exists in the table, this symbol is returned. Otherwise, a new symbol is created with string as its name; this symbol is entered into the table and returned.

    The effect of modifying a string after it is used as an argument to string->symbol is unspecified.

    (string->symbol "x") <graphic> x 

    (eq? (string->symbol "x") 'x) <graphic> #t
    (eq? (string->symbol "X") 'x) <graphic> #f 

    (eq? (string->symbol "x")
        (string->symbol "x")) <graphic> #t 

    (string->symbol "()") <graphic> \x28;\x29;

    procedure: (symbol->string symbol) 
    returns: a string, the name of symbol 
    libraries: (rnrs base), (rnrs)

    The string returned by symbol->string should be treated as immutable. 
    Unpredictable behavior can result if a string passed to string->symbol is altered with string-set! or by any other means.

    (symbol->string 'xyz) <graphic> "xyz"
    (symbol->string 'Hi) <graphic> "Hi"
    (symbol->string (string->symbol "()")) <graphic> "()"

  Section 6.12. Booleans

    While every Scheme object has a truth value when used in a conditional context, with every object but #f counting as true, 
    Scheme provides the dedicated true value #t for use when a value of an expression should convey nothing more than that it is true.

    procedure: (boolean=? boolean1 boolean2) 
    returns: #t if the two booleans are the same, #f otherwise 
    libraries: (rnrs base), (rnrs)

    The boolean values #t and #f may also be compared with eq?, which is typically more efficient than boolean=?.

    (boolean=? #t #t) <graphic> #t
    (boolean=? #t #f) <graphic> #f
    (boolean=? #t (< 3 4)) <graphic> #t

  Section 6.13. Hashtables

    Hashtables represent sets of associations between arbitrary Scheme values. 
    They serve essentially the same purpose as association lists (see page  165) but are typically much faster when large numbers of associations are involved.

    procedure: (make-eq-hashtable) 
    procedure: (make-eq-hashtable size) 
    returns: a new mutable eq hashtable 
    libraries: (rnrs hashtables), (rnrs)

    If size is provided, it must be a nonnegative exact integer indicating approximately how many elements the hashtable should initially hold. 
    Hashtables grow as needed, but when the hashtable grows it generally must rehash all of the existing elements. 
    Providing a nonzero size can help limit the amount of rehashing that must be done as the table is initially populated.

    An eq hashtable compares keys using the eq? (pointer equality) procedure and typically employs a hash function based on object addresses. 
    Its hash and equivalence functions are suitable for any Scheme object.

    (define ht1 (make-eq-hashtable))
    (define ht2 (make-eq-hashtable 32))

    procedure: (make-eqv-hashtable) 
    procedure: (make-eqv-hashtable size) 
    returns: a new mutable eqv hashtable 
    libraries: (rnrs hashtables), (rnrs)

    If size is provided, it must be a nonnegative exact integer indicating approximately how many elements the hashtable should initially hold. 
    Hashtables grow as needed, but when the hashtable grows it generally must rehash all of the existing elements. 
    Providing a nonzero size can help limit the amount of rehashing that must be done as the table is initially populated.

    An eqv hashtable compares keys using the eqv? procedure and typically employs a hash function based on object addresses for objects that are identifiable with eq?. Its hash and equivalence functions are suitable for any Scheme object.

    procedure: (make-hashtable hash equiv?) 
    procedure: (make-hashtable hash equiv? size) 
    returns: a new mutable hashtable 
    libraries: (rnrs hashtables), (rnrs)

    hash and equiv? must be procedures. If size is provided, it must be a nonnegative exact integer indicating approximately how many elements the hashtable should initially hold. 
    Hashtables grow as needed, but when the hashtable grows it generally must rehash all of the existing elements. 
    Providing a nonzero size can help limit the amount of rehashing that must be done as the table is initially populated.

    The new hashtable computes hash values using hash and compares keys using equiv?, neither of which should modify the hashtable. 
    equiv? should compare two keys and return false only if the two keys should be distinguished. 
    hash should accept a key as an argument and return a nonnegative exact integer value that is the same each time it is called with arguments that equiv? does not distinguish. 
    The hash and equiv? procedures need not accept arbitrary inputs as long as the hashtable is used only for keys that they do accept, 
    and both procedures may assume that the keys are immutable as long as the keys are not modified while they have associations stored in the table. 
    The hashtable operation may call hash and equiv? once, not at all, or multiple times for each hashtable operation.

    (define ht (make-hashtable string-hash string=?))

    procedure: (hashtable-mutable? hashtable) 
    returns: #t if hashtable is mutable, #f otherwise 
    libraries: (rnrs hashtables), (rnrs)

    Hashtables returned by one of the hashtable creation procedures above are mutable, but those created by hashtable-copy may be immutable. 
    Immutable hashtables cannot be altered by any of the procedures hashtable-set!, hashtable-update!, hashtable-delete!, or hashtable-clear!.

    (hashtable-mutable? (make-eq-hashtable)) <graphic> #t
    (hashtable-mutable? (hashtable-copy (make-eq-hashtable))) <graphic> #f

    procedure: (hashtable-hash-function hashtable) 
    returns: the hash function associated with hashtable 
    procedure: (hashtable-equivalence-function hashtable) 
    returns: the equivalence function associated with hashtable 
    libraries: (rnrs hashtables), (rnrs)

    hashtable-hash-function returns #f for eq and eqv hashtables.

    (define ht (make-eq-hashtable))
    (hashtable-hash-function ht) <graphic> #f
    (eq? (hashtable-equivalence-function ht) eq?) <graphic> #t 

    (define ht (make-hashtable string-hash string=?))
    (eq? (hashtable-hash-function ht) string-hash) <graphic> #t
    (eq? (hashtable-equivalence-function ht) string=?) <graphic> #t

    procedure: (equal-hash obj) 
    procedure: (string-hash string) 
    procedure: (string-ci-hash string) 
    procedure: (symbol-hash symbol) 
    returns: an exact nonnegative integer hash value 
    libraries: (rnrs hashtables), (rnrs)

    These procedures are hash functions suitable for use with the appropriate Scheme predicate: equal? for equal-hash, string=? for string-hash, string-ci=? for string-ci-hash, and symbol=? (or eq?) for symbol-hash. 
    The hash values returned by equal-hash, string-hash, and string-ci-hash are typically dependent on the current structure and contents of the input values and are thus unsuitable if keys are modified while they have associations in a hashtable.

    procedure: (hashtable-set! hashtable key obj) 
    returns: unspecified 
    libraries: (rnrs hashtables), (rnrs)

    hashtable must be a mutable hashtable. key should be an appropriate key for the hashtable's hash and equivalence functions. obj may be any Scheme object.

    hashtable-set! associates key with obj in hashtable, replacing the existing association, if any.

    (define ht (make-eq-hashtable))
    (hashtable-set! ht 'a 73)

    procedure: (hashtable-ref hashtable key default) 
    returns: see below 
    libraries: (rnrs hashtables), (rnrs)

    key should be an appropriate key for the hashtable's hash and equivalence functions. default may be any Scheme object.

    hashtable-ref returns the value associated with key in hashtable. If no value is associated with key in hashtable, hashtable-ref returns default.

    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b)) 

    (define eqht (make-eq-hashtable))
    (hashtable-set! eqht p1 73)
    (hashtable-ref eqht p1 55) <graphic> 73
    (hashtable-ref eqht p2 55) <graphic> 55 

    (define equalht (make-hashtable equal-hash equal?))
    (hashtable-set! equalht p1 73)
    (hashtable-ref equalht p1 55) <graphic> 73
    (hashtable-ref equalht p2 55) <graphic> 73

    procedure: (hashtable-contains? hashtable key) 
    returns: #t if an association for key exists in hashtable, #f otherwise 
    libraries: (rnrs hashtables), (rnrs)

    key should be an appropriate key for the hashtable's hash and equivalence functions.

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b))
    (hashtable-set! ht p1 73)
    (hashtable-contains? ht p1) <graphic> #t
    (hashtable-contains? ht p2) <graphic> #f

    procedure: (hashtable-update! hashtable key procedure default) 
    returns: unspecified 
    libraries: (rnrs hashtables), (rnrs)

    hashtable must be a mutable hashtable. key should be an appropriate key for the hashtable's hash and equivalence functions. default may be any Scheme object. procedure should accept one argument, should return one value, and should not modify hashtable.

    hashtable-update! applies procedure to the value associated with key in hashtable, or to default if no value is associated with key in hashtable. If procedure returns, hashtable-update! associates key with the value returned by procedure, replacing the old association, if any.

    A version of hashtable-update! that does not verify that it receives arguments of the proper type might be defined as follows.

    (define hashtable-update!
      (lambda (ht key proc value)
        (hashtable-set! ht key
          (proc (hashtable-ref ht key value)))))

    An implementation may, however, be able to implement hashtable-update! more efficiently by avoiding multiple hash computations and hashtable lookups.

    (define ht (make-eq-hashtable))
    (hashtable-update! ht 'a
      (lambda (x) (* x 2))
      55)
    (hashtable-ref ht 'a 0) <graphic> 110
    (hashtable-update! ht 'a
      (lambda (x) (* x 2))
      0)
    (hashtable-ref ht 'a 0) <graphic> 220

    procedure: (hashtable-delete! hashtable key) 
    returns: unspecified 
    libraries: (rnrs hashtables), (rnrs)

    hashtable must be a mutable hashtable. key should be an appropriate key for the hashtable's hash and equivalence functions.

    hashtable-delete! drops any association for key from hashtable.

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b))
    (hashtable-set! ht p1 73)
    (hashtable-contains? ht p1) <graphic> #t
    (hashtable-delete! ht p1)
    (hashtable-contains? ht p1) <graphic> #f
    (hashtable-contains? ht p2) <graphic> #f
    (hashtable-delete! ht p2)

    procedure: (hashtable-size hashtable) 
    returns: number of entries in hashtable 
    libraries: (rnrs hashtables), (rnrs)

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b))
    (hashtable-size ht) <graphic> 0
    (hashtable-set! ht p1 73)
    (hashtable-size ht) <graphic> 1
    (hashtable-delete! ht p1)
    (hashtable-size ht) <graphic> 0

    procedure: (hashtable-copy hashtable) 
    procedure: (hashtable-copy hashtable mutable?) 
    returns: a new hashtable containing the same entries as hashtable 
    libraries: (rnrs hashtables), (rnrs)

    If mutable? is present and not false, the copy is mutable; otherwise, the copy is immutable.

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (hashtable-set! ht p1 "c")
    (define ht-copy (hashtable-copy ht))
    (hashtable-mutable? ht-copy) <graphic> #f
    (hashtable-delete! ht p1)
    (hashtable-ref ht p1 #f) <graphic> #f
    (hashtable-delete! ht-copy p1) <graphic> exception: not mutable
    (hashtable-ref ht-copy p1 #f) <graphic> "c"

    procedure: (hashtable-clear! hashtable) 
    procedure: (hashtable-clear! hashtable size) 
    returns: unspecified 
    libraries: (rnrs hashtables), (rnrs)

    hashtable must be a mutable hashtable. If size is provided, it must be a nonnegative exact integer.

    hashtable-clear! removes all entries from hashtable. If size is provided, the hashtable is reset to the given size, as if newly created by one of the hashtable creation operations with size argument size.

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b))
    (hashtable-set! ht p1 "first")
    (hashtable-set! ht p2 "second")
    (hashtable-size ht) <graphic> 2
    (hashtable-clear! ht)
    (hashtable-size ht) <graphic> 0
    (hashtable-ref ht p1 #f) <graphic> #f

    procedure: (hashtable-keys hashtable) 
    returns: a vector containing the keys in hashtable 
    libraries: (rnrs hashtables), (rnrs)

    The keys may appear in any order in the returned vector.

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b))
    (hashtable-set! ht p1 "one")
    (hashtable-set! ht p2 "two")
    (hashtable-set! ht 'q "three")
    (hashtable-keys ht) <graphic> #((a . b) q (a . b))

    procedure: (hashtable-entries hashtable) 
    returns: two vectors: one of keys and a second of values 
    libraries: (rnrs hashtables), (rnrs)

    hashtable-entries returns two values. The first is a vector containing the keys in hashtable, and the second is a vector containing the corresponding values. The keys and values may appear in any order, but the order is the same for the keys and for the corresponding values.

    (define ht (make-eq-hashtable))
    (define p1 (cons 'a 'b))
    (define p2 (cons 'a 'b))
    (hashtable-set! ht p1 "one")
    (hashtable-set! ht p2 "two")
    (hashtable-set! ht 'q "three")
    (hashtable-entries ht) <graphic> #((a . b) q (a . b))
                            #("two" "three" "one")

  Section 6.14. Enumerations

    ...
    ...
    ...





