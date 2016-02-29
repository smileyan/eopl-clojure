; 3. Expression

; 3.1 Specification and Implementation Strategy

; (value-of exp ρ) = val ; the value of expression exp in enviroment ρ should be val.

3.2 LET: A Simple Language

Syntax for the LET language

Program    ::= Expression
               a-program (exp1)
Expression ::= Number
               const-exp (num)
Expression ::= -(Expression , Expression)
               diff-exp (exp1 exp2)
Expression ::= zero? (Expression)
               zero?-exp (exp1)
Expression ::= if Expression then Expression else Expression
               if-exp ( exp1 exp2 exp3)
Expression ::= Identifier
               var-exp (var)
Expression ::= let Identifier = Expression in Expression
               let-exp (var exp1 body)

3.2.1 Specifying the Syntax

(scan&parse "-(55, -(x,11))")
# (struct:a-program
    #(struct:diff-exp
       #(struct:const-exp 55)
       #(struct:diff-exp
          #(struct:var-exp x)
          #(struct:const-exp 11))))

3.2.2 Specification of Values

ExpVal = Int + Bool
DenVal = Int + Bool

Entries of this interface

num-val      : Int -> ExpVal
bool-val     : Bool -> ExpVal
expval->num  : ExpVal -> Int
expval->bool : ExpVal -> Bool

3.2.3 Environments