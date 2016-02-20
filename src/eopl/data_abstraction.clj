(ns eopl.data-abstraction
  (:require [eopl.common :refer :all]))

; (zero) = [0]
; (is-zero? [0]) = #t when n = 0
;                  #f when n ≠ 0
; (successor [n]) = [n + 1] (n ≥ 0)
; (predecessor [n + 1]) = [n] (n ≥ 0)

; (plus [x] [y]) = [x + y]
(declare successor)
(declare predecessor)
(defn plus [x y]
  (if (zero? x)
    y
    (successor (plus (predecessor x) y))))

; Unary representation
; [0] = ()
; [n + 1] = (#t . [n])

(defn zero []
;   '())
  0)

(defn is-zero? [n]
;   (empty? n))
  (= n 0))

(defn successor [n]
;   (cons true n))
  (+ n 1))

(defn predecessor [n]
;   (cdr n) )
  (- n 1))

; Scheme number representation:

; t ::=                                                 terms:
;        true                                    constant true
;        false                                  constant false
;        if t then t else t                        conditional
;        0                                       constant zero
;        succ t                                      successor
;        pred t                                    predecessor
;        iszero t                                    zero test

; inductive definition:
; 1. {true,false,0} ⊆ T;
; 2. if t1 ∈ T, then {succ t1, pred t1, iszero t1} ⊆ T;
; 3. if t1 ∈ T, t2 ∈ T, and t3 ∈ T, then if t1 then t2 else t3

; inference rule:
; true ∈ T                   false ∈ T                    0 ∈ T

;     t1 ∈ T                t1 ∈ T                    t1 ∈ T
; ---------------      -----------------         ------------------
;   succ t1 ∈ T           pred t1 ∈ T              iszero t1 ∈ T

; t1 ∈ T   t2 ∈ T  t3 ∈ T
; -------------------------
; if t1 then t2 else t3 ∈ T

; S0   = ∅
; Si+1 =    {true, false, 0}
;         ∪ {succ t1, pred t1, iszero t1 | t1 ∈ Si}
;         ∪ {if t1 then t2 else t3 | t1,t2,t3 ∈ Si}.
; Finally, let
;        S  = ∪Si.
;             i 

; Bignum representation
; [n] = ()        n = 0
;     | (r . [q]) n = qN + r, 0 ≤ r < N

; N = 16
; [33] = (1 2)     [258] = (2 0 1)

; 2.2.1 The Environment Interface
; (empty-env)             = [∅]
; (apply-env [f] var)     = f(var)
; (extend-env var v [f])  = [g],
;                         where g(var1) = v if var1 = var
;                                              | f(var1) otherwise

; empty-env and extend-env are the constructors
; apply-env is the only observer

; 2.2.2 Data Structure Representation

; Env-exp ::= (empty-env)
;         ::= (extend-env Identifier Scheme-value Env-exp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Env = (empty-env) | (extend-env Var SchemeVal Env)
; Var = Sym

; empty-env : () -> Env
(defn empty-env []
  (list 'empty-env))


; extend-env : Var * SchemeVal * Env -> Env
(defn extend-env [var val env]
  (list 'extend-env var val env))

; apply-env : Env * Var -> SchemeVal
(defn apply-env [env search-var]
  (cond
    (= (car env) 'empty-env) (report-no-binding-found search-var)
    (= (car env) 'extend-env) 
      (let [saved-var (cadr env)
            saved-val (caddr env)
            saved-env (cadddr env)]
        (if (= search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))
    :else (report-invalid-env env)))

; ∀x∈S: x~x 
; ∀x ∈ A, f(x) = x
; f: A -> A
;    x -> x
; (lambda x: x)
; (f a)
; ((lambda x: x) a)

; 2.2.3 Procedural Representation

; Env = Var -> SchemeVal

; empty-env : () -> Env

(defn empty-env-pr []
  (fn [search-var]
    (report-no-binding-found search-var)))

; extend-env : Var * SchemeVal * Env -> search-var
(defn extend-env-pr [saved-var saved-val saved-env]
  (fn [search-var]
    (if (= search-var saved-var)
      saved-var
      (apply-env saved-env search-var))))

; apply-env : Env * Var -> SchemeVal
(defn apply-env-pr [env search-var]
  (env search-var))

; 2.3 Interfaces for Recursive Data Types

; Lc-exp::=Identifier
;       ::=(lambda (Identifier) Lc-exp)
;       ::=(Lc-exp Lc-exp)