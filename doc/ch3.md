# Chapter 3. Going Further
    Section 3.1. Syntactic Extension
        <program>	           --> 	<form>*
        <form>	               -->  <definition> | <expression>
        <definition>           -->  <variable definition> | (begin <definition>*)
        <variable definition>  -->	(define <variable> <expression>)
        <expression>           -->  <constant>
                                |	<variable>
                                |	(quote <datum>)
                                |	(lambda <formals> <expression> <expression>*)
                                |	(if <expression> <expression> <expression>)
                                |	(set! <variable> <expression>)
                                |	<application>
        <constant>	           -->	<boolean> | <number> | <character> | <string>
        <formals>              -->	<variable>
                                |	(<variable>*)
                                |	(<variable> <variable>* . <variable>)
        <application>	       -->	(<expression> <expression>*)