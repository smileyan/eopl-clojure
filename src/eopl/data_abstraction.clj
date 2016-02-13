; (zero) = [0]
; (is-zero? [0]) = #t when n = 0
;                  #f when n ≠ 0
; (successor [n]) = [n + 1] (n ≥ 0)
; (predecessor [n + 1]) = [n] (n ≥ 0)

; (plus [x] [y]) = [x + y] 
(defn plus [x y]
  (if (is-zero? x)
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

(def successor [n]
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