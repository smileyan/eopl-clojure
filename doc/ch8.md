Chapter 8. Syntactic Extension

    Syntactic extensions, or macros, are used to simplify and regularize repeated patterns in a program, 
    to introduce syntactic forms with new evaluation rules, and to perform transformations that help make programs more efficient.

    A syntactic extension most often takes the form (keyword subform ...), where keyword is the identifier that names the syntactic extension. 
    The syntax of each subform varies from one syntactic extension to another. Syntactic extensions can also take the form of improper lists or even singleton identifiers.

    New syntactic extensions are defined by associating keywords with transformation procedures, or transformers. 
    Syntactic extensions are defined using define-syntax forms or using let-syntax or letrec-syntax. 
    Transformers may be created using syntax-rules, which allows simple pattern-based transformations to be performed. 
    They may also be ordinary procedures that accept one argument and perform arbitrary computations. 
    In this case, syntax-case is normally used to destructure the input and syntax is normally used to construct the output. 
    The identifier-syntax form and make-variable-transformer procedure allow the creation of transformers that match singleton identifiers and assignments to those identifiers, 
    the former being restricted to simple patterns like syntax-rules and the latter allowing arbitrary computations to be performed.

    Syntactic extensions are expanded into core forms at the start of evaluation (before compilation or interpretation) by a syntax expander. 
    If the expander encounters a syntactic extension, it invokes the associated transformer to expand the syntactic extension, 
    then repeats the expansion process for the form returned by the transformer. 
    If the expander encounters a core syntactic form, it recursively processes the subforms, if any, and reconstructs the form from the expanded subforms. 
    Information about identifier bindings is maintained during expansion to enforce lexical scoping for variables and keywords.

    The syntactic extension mechanisms described in this chapter are part of the "syntax-case" system. 
    A portable implementation of the system that also supports libraries and top-level programs is available at http://www.cs.indiana.edu/syntax-case/. 
    A description of the motivations behind and implementation of the system can be found in the article "Syntactic Abstraction in Scheme" [12]. 
    Additional features that have not yet been standardized, including modules, local import, and meta definitions, are described in the Chez Scheme User's Guide [9].

Section 8.1. Keyword Bindings

    This section describes forms that establish bindings between keywords and transformers. 
    Keyword bindings may be established within a top-level program or library body using define-syntax and 
    in any local scope using define-syntax, let-syntax, or letrec-syntax.

    syntax: (define-syntax keyword expr) 
    libraries: (rnrs base), (rnrs)

    expr must evaluate to a transformer.

    The following example defines let* as a syntactic extension, specifying the transformer with syntax-rules (see Section 8.2).

    (define-syntax let*
      (syntax-rules ()
        [(_ () b1 b2 ...) (let () b1 b2 ...)]
        [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
         (let ([i1 e1])
           (let* ([i2 e2] ...) b1 b2 ...))]))

    All bindings established by a set of internal definitions, whether keyword or variable definitions, 
    are visible everywhere within the immediately enclosing body, including within the definitions themselves. For example, the expression

    (let ()
      (define even?
        (lambda (x)
          (or (= x 0) (odd? (- x 1)))))
      (define-syntax odd?
        (syntax-rules ()
          [(_ x) (not (even? x))]))
      (even? 10))

    is valid and should evaluate to #t.

    The expander processes the initial forms in a library, lambda, or other body from left to right. 
    If it encounters a variable definition, it records the fact that the defined identifier is a variable but defers expansion of the right-hand-side expression 
    until after all of the definitions have been processed. 
    If it encounters a keyword definition, it expands and evaluates the right-hand-side expression and binds the keyword to the resulting transformer. 
    If it encounters an expression, it fully expands all deferred right-hand-side expressions along with the current and remaining body expressions.

    An implication of the left-to-right processing order is that one internal definition can affect whether a subsequent form is also a definition. For example, the expression

    (let ()
      (define-syntax bind-to-zero
        (syntax-rules ()
          [(_ id) (define id 0)]))
      (bind-to-zero x)
      x)

