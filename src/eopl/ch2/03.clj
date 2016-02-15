; Diff-tree::=(one) | (diff Diff-tree Diff-tree)
(defn zero []
  '(diff (one) (one)))

(defn is-zero? [n]
  (= zero n))

(defn successor [n]
  '(diff (n) (diff (diff (one) (one)) (one))))

(defn predecessor [n]
  '(diff (n) (one)))