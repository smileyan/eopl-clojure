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


