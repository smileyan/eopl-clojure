(ns eopl.ch1.07)

(defn nth-element-rec [lst n]
  (if (empty? lst)
    false
    (if (zero? n)
      (first lst)
      (recur (rest lst) (- n 1)))))

(defn nth-element [lst n]
    (let [ans (nth-element-rec lst n)]
      (if (not ans)
	    (print "error")
	    ans)))