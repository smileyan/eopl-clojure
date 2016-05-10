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


