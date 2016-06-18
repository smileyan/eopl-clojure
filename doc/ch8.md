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
