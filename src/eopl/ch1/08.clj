(ns eopl.ch1.08)

; remove-first : Sym * Listof(Sym)  ->  Listof(Sym)
; usage:  (remove-first s los) returns a list with
;         the same elements arranged in the same
;         order as los, except the elements before
;         the first occurrence of the symbol s is removed.
(defn remove-first [s los]
  (if (empty? los)
    '()
    (if (= (first los) s)
      (rest los)
      (remove-first s (rest los)))))