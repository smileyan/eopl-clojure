(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(defn bintree-to-list [btree]
  (cases bintree btree
    (leaf-node (num)
      (list 'leaf-node num))
    (interior-node (key left right)
      (list 'interior-node
            key
            (bintree-to-list left)
            (bintree-to-list right)))))
