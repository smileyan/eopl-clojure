(ns eopl.ch1.27
  (:require [eopl.common :refer :all]))

; flatten: Listof(Sym | Listof(Sym)) -> Listof(Sym)
; usage : (flatten slist) returns a list of the symbols contained in
; slist in the order in which they occur when slist is printed. Intuitively, flatten
; removes all the inner parentheses from its argument.

; (defn my-flatten [slist]
;   (cond
;     (symbol? slist) (list slist)
;     (empty? slist) '()
;     (seq? slist) (into (my-flatten (cdr slist)) 
;                        (my-flatten (car slist)))))

(declare flatten-with-tail)
(defn my-flatten [slist]
  (flatten-with-tail slist '()))

(defn flatten-with-tail [slist tail]
  (cond
    (empty? slist) tail
    (symbol? (car slist)) (cons (car slist) 
                                (flatten-with-tail (cdr slist) 
                                                   tail))
    :else (flatten-with-tail (car slist) 
                             (flatten-with-tail (cdr slist) 
                                                tail))))