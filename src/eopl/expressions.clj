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

An enviroment is a function whose domain is a finite set of variables and whose range is the denoted values.

 ρ ranges over enviroments.
 [] denotes the empty enviroment.
 [var = val]ρ denotes (extend-env var val ρ).
 [var1 = val1, var2 = val2]ρ abbreviates [var1 = val1]([var2 = val2]ρ), etc.
 [var1 = val1, var2 = val2,...] denotes the enviroment in which the values of var1 is val1,etc.
 
 [x=3]
  [y=7]
   [u=5]ρ
 to abbreviates
 (extend-env 'x 3
   (extend-env 'y 7
     (extend-env 'u 5 ρ)))

3.2.4 Specifying the Behavior of Expressions

  constructors:
  const-exp   : Int -> Exp
  zero?-exp   : Exp -> Exp
  if-exp      : Exp*Exp*Exp -> Exp
  diff-exp    : Exp*Exp -> Exp
  var-exp     : Var -> Exp
  let-exp     : Var*Exp*Exp -> Exp
  
  observer:
  value-of    : Exp*Env -> ExpVal

(value-of (const-exp n) ρ) = (num-val n)
(value-of (var-exp var) ρ) = (apply-env ρ var)
(value-of (diff-exp exp1 exp2) ρ)
= (num-val
    (-
      (expval->num (value-of exp1 ρ))
      (expval->num (value-of exp2 ρ))))

3.2.5 Specifying the Behavior of Programs

(value-of-program exp)
= (value-of exp [i=⌈1⌉,v=⌈5⌉,x=⌈10⌉])

3.2.6 Specifying Conditionals

        (value-of exp1 ρ) = val1
__________________________________________________
(value-of (zero?-exp exp1) ρ)
  (bool-val #t)    if (expval->num  val1) = 0
= (bool-val #f)    if (expval->num  val1) ≠ 0

        (value-of exp1 ρ) = val1
__________________________________________________
(value-of (if-exp exp1 exp2 exp3) ρ)
  (value-of exp2 ρ)    if (expval->bool  val1) = #t
= (value-of exp3 ρ)    if (expval->bool  val1) = #f

For an if-exp, the equational specification is
(value-of (if-exp exp1 exp2 exp3) ρ)
= (if (expval->bool (value-of exp1 ρ))
    (value-of exp2 ρ)
    (value-of exp3 ρ))

3.2.7 Specifying let

let x = 5
in -(x,3)

let z = 5
in let x = 3
   in let y = -(x,1)      % here x = 3
      in let x = 4
         in -(z, -(x,y))  % here x = 4