(ns eopl.ch1.15)

; duple : Int*Sym  ->  Listof(Sym)
; usage:  (duple n x) returns a list with
;         n copies if x
(defn duple [n x]
  (if (zero? n)
    '()
    (cons x
          (duple (- n 1) x))))
