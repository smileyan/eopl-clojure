#lang eopl

; 1.1 Recursively Specified Data

; 1.1.1 Inductive Specification

; top-down
; Definition 1.1.1 A natural number n is in S iff
; 1. n = 0, or
; 2. n - 3 is in S.

; in-S? : N -> Bool
; usage
(define in-S?
  (lambda (n)
    (if (zero? n) #t
      (if (>= (- n 3) 0)
        (in-S? (- n 3))
        #f))))

; botton up
; Definition 1.1.2 Define the set S to be the smallest set contained in N and satisfy-
; ing the following two properties:
; 1. 0 is in S, and
; 2. if n is in S, then n + 3 is in S.

; rules-of-inference
        ;    _________
        ;    0 is in S

        ;    n is in S
        ; ---------------
        ; (n + 3) is in S

; Definition 1.1.3(list of integers, top-down) A Scheme list is a list of integers
; iff either
; 1. it is a empty list, or
; 2. it is a pair whose car is an integer and cdr is a list of integers.

; Definition 1.1.4(list of integers, botton-down) The set of List-of-Int is the smallest
; set of Scheme lists satisfying the following two properties:
; 1. () is in List-of-Int, and
; 2. if n is in Int and L is in List-of-Int, then (n . l) is in List-of-Int.

; Definition 1.1.5(list of integers, rules of inference)
;                       0 is in List-of-Int
                      
;                   n is in Int    l is in List-of-Int
;                   ----------------------------------
;                         (n . l) is in List-of-Int
                        