(define-datatype env env?
  (empty-env)
  (extend-env
    (saved-var var?)
    (saved-val scheme-value?)
    (saved-env env?)))

(defn var? [v]
  (symbol? v))

(defn scheme-value? [s]
  true)

(defn apply-env [environment search-var]
  (cases env environment
    (empty-env ()
      (report-no-binding-found search-var))
    (extend (saved-var saved-val saved-env)
      (if (= search-var saved-var)
        saved-val
        (apply-env saved-env search-var)))))

(defn has-binding? [environment search-var]
  (cases env environment
    (empty-env ()
      false)
    (extend-env (saved-var saved-val saved-env)
      (or (= search-var saved-var)
          (has-binding? saved-env search-var)))))
