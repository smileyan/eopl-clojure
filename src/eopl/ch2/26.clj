; Red-blue-tree    ::= Red-blue-subtree
; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;                  ::= (blue-node {Red-blue-subtree}*)
;                  ::= (leaf-node Int)

(define-datatype red-blue-tree red-blue-tree?
  (a-red-blue-tree
    (root red-blue-subtree?)))

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
    (left red-blue-subtree?)
    (right red-blue-subtree?))
  (blue-node
    (subtrees (list-of red-blue-subtree?)))
  (leaf-node
    (num integer?)))
