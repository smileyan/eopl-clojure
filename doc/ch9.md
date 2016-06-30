Chapter 9. Records

    This chapter describes the means by which the programmer may define new data types, or records types, each distinct from all other types. 
    A record type determines the number and names of the fields each instance of the type has. 
    Records are defined via the define-record-type form or the make-record-type-descriptor procedure.

  Section 9.1. Defining Records

    A define-record-type form defines a record type and, along with it, a constructor procedure for records of the type, 
    a type predicate that returns true only for records of the type, an access procedure for each field, and an assignment procedure for each mutable field. 
    For example, the definition

    (define-record-type point (fields x y))

    creates a point record type with two fields, x and y, and defines the following procedures:

    (make-point x y)	constructor
    (point? obj)	predicate
    (point-x p)	accessor for field x
    (point-y p)	accessor for field y
    With this definition in place, we can use these procedures to create and manipulate records of the point type, as illustrated below.

    (define p (make-point 36 -17))
    (point? p) <graphic> #t
    (point? '(cons 36 -17)) <graphic> #f
    (point-x p) <graphic> 36
    (point-y p) <graphic> -17

    Fields are immutable by default, but may be declared mutable. 
    In the alternate definition of point below, the x field is mutable while y remains immutable.

    (define-record-type point (fields (mutable x) y))

    In this case, define-record-type defines a mutator for the x field in addition to the other products shown above.

    (point-x-set! p x)	mutator for field x
    The mutator can be used to change the contents of the x field.

    (define p (make-point 36 -17))
    (point-x-set! p (- (point-x p) 12))
    (point-x p) <graphic> 24

    A field may be declared immutable explicitly for clarity; the definition of point below is equivalent to the second definition above.

    (define-record-type point (fields (mutable x) (immutable y)))

    The names of the procedures defined by define-record-type follow the regular naming convention illustrated by the examples above, 
    by default, but the programmer can override the defaults if desired. 
    With the following definition of point, the constructor is mkpoint, the predicate is ispoint?, 
    and the accessors for x and y are x-val and y-val. The mutator for x is set-x-val!.

    (define-record-type (point mkpoint ispoint?)
      (fields (mutable x x-val set-x-val!)
              (immutable y y-val)))

    By default, a record definition creates a new type each time it is evaluated, as illustrated by the example below.

    (define (f p)
      (define-record-type point (fields x y))
      (if (eq? p 'make) (make-point 3 4) (point? p)))
    (f (f 'make)) <graphic> #f

    The first (inner) call to f returns a point p, which is passed to f in the second (outer) call, which applies point? to p. 
    This point? is looking for points of the type created by the second call, while p is a point of the type created by the first call. So point? returns #f.

    This default generative behavior may be overridden by including a nongenerative clause in the record definition.

    (define (f p)
      (define-record-type point (fields x y) (nongenerative))
      (if (eq? p 'make) (make-point 3 4) (point? p)))
    (define p (f 'make))
    (f p) <graphic> #t

    Record types created in this manner are still distinct from record types created by a definition appearing in a different part of the program, even if the definitions are syntactically identical:

    (define (f)
      (define-record-type point (fields x y) (nongenerative))
      (make-point 3 4))
    (define (g p)
      (define-record-type point (fields x y) (nongenerative))
      (point? p))
    (g (f)) <graphic> #f

    Even this can be overridden by including a uid (unique id) in the nongenerative clause:

    (define (f)
      (define-record-type point (fields x y)
        (nongenerative really-the-same-point))
      (make-point 3 4))
    (define (g p)
      (define-record-type point (fields x y)
        (nongenerative really-the-same-point))
      (point? p))
    (g (f)) <graphic> #t

    The uid may be any identifier, but programmers are encouraged to select uids from the RFC 4122 UUID namespace [20], possibly with the record-type name as a prefix.

