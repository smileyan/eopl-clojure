(ns eopl.ch1.09)

; remove : Sym * Listof(Sym)  ->  Listof(Sym)
; usage:  (remove s los) returns a list with
;         the same elements arranged in the same
;         order as los, except all the s in los is removed.
(defn remove-all [s los]
  (if (empty? los)
    '()
    (if (= (first los) s)
      (remove-all s (rest los))
      (cons (first los) (remove-all s (rest los)))
      )))
