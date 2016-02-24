
(define-datatype stack stack?
  (empty-stack)
  (push
    (val value?)
    (s stack?)))

(defn value? [s]
  true)

(defn pop [s]
  (cases stack s
    (empty-stack ()
      (report-empty-stack 'pop))
    (push (saved-val saved-stack)
      saved-stack)))

(defn top [s]
  (cases stack s
    (empty-stack ()
      (report-empty-stack 'top))
    (push (saved-val saved-stack)
      saved-val)))

(defn empty-stack? [s]
  (cases stack s
    (empty-stack () true)
    (push (saved-val saved-stack) false)))