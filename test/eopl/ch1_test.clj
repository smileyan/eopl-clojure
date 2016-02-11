(ns eopl.ch1-test
  (:require [clojure.test :refer :all]
            [eopl.common :refer :all]
            [eopl.ch1.07 :refer :all]
            [eopl.ch1.08 :refer :all]
            [eopl.ch1.09 :refer :all]
            [eopl.ch1.12 :refer :all]
            [eopl.ch1.13 :refer :all]
            [eopl.ch1.15 :refer :all]
            [eopl.ch1.16 :refer :all]
            [eopl.ch1.17 :refer :all]
            [eopl.ch1.18 :refer :all]
            [eopl.ch1.19 :refer :all]
            [eopl.ch1.20 :refer :all]
            [eopl.ch1.21 :refer :all]
            [eopl.ch1.22 :refer :all]
            [eopl.ch1.23 :refer :all]
            [eopl.ch1.24 :refer :all]
            [eopl.ch1.25 :refer :all]
            [eopl.ch1.26 :refer :all]
            [eopl.ch1.27 :refer :all]
            [eopl.ch1.28 :refer :all]
            [eopl.ch1.29 :refer :all]
            [eopl.ch1.30 :refer :all]
            [eopl.ch1.31 :refer :all]
            [eopl.ch1.32 :refer :all]
            [eopl.ch1.33 :refer :all]
            [eopl.ch1.34 :refer :all]
            ))

(deftest nth-element-test
  (testing "nth element test"
    (is (= 2 (nth-element '(1 2 3) 1)))
    (is (= 1 1))))

(deftest remove-first-test
  (testing "remove first"
    (is (= '(b c) (remove-first 'a '(a b c))))
    (is (= '() (remove-first 'b '(e f g))))
    (is (= '(c1 a4) (remove-first 'a4 '(c1 a4 c1 a4))))
    (is (= '() (remove-first 'x '())))
    ))

(deftest remove-test
  (testing "remove test"
    (is (= '(b c e) (remove-all 'a '(a a b c e a))))
    (is (= '() (remove-all 'a '(a a))))
    ))

(deftest subst-inlining-test
  (testing "subst-inlining test"
    (is (= '(a a c d e) (subst-inlining 'a 'b '(a b c d e))))
    (is (= '(a) (subst-inlining 'a 'b '(b))))
    (is (= '(a a a) (subst-inlining 'a 'b '(b b b))))
    (is (= '((s b) c d s) (subst-inlining 's 'a '((a b) c d s))))
    ))

(deftest subst-map-test
  (testing "subst-map test"
    (is (= '(a a c d e) (subst-map 'a 'b '(a b c d e))))
    (is (= '(a) (subst-map 'a 'b '(b))))
    (is (= '(a a a) (subst-map 'a 'b '(b b b))))
    (is (= '((s b) c d s) (subst-map 's 'a '((a b) c d s))))
    ))

(deftest duple-test
  (testing "(duple n x) test"
    (is (= (duple 2 3) '(3 3)))
    (is (= (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha))))
    (is (= (duple 0 '(blha)) '()))
    ))

(deftest invert-lst-test
  (testing "invert lst test"
    (is (= '((1 a) (2 a) (b 1) (b 2)) (invert '((a 1) (a 2) (1 b) (2 b)))))
    ))

(deftest down-lst-test
  (testing "down lst test"
    (is (= '((1) (2) (3)) (down '(1 2 3))))
    (is (= '(((a)) ((fine)) ((idea))) (down '((a) (fine) (idea)))))
    (is (= '((a) ((more (complicated))) (object)) (down '(a (more (complicated)) object))))
    ))

(deftest swapper-test
  (testing "(swapper s1 s2 slist)"
    (is (= '(d b c a) (swapper 'a 'd '(a b c d))))
    (is (= '(d a () c a) (swapper 'a 'd '(a d () c d))))
    (is (= '((y) x (z (y))) (swapper 'x 'y '((x) y (z (x))))))
    ))

(deftest list-set-test
  (testing "(list-set lst n x"
    (is (= (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d)))
    ))

(deftest count-occurrences-test
  (testing "(count-occurrences s slist)"
    (is (= 3 (count-occurrences 'x '((f x) y (((x z) x))))))
    (is (= 3 (count-occurrences 'x '((f x) y (((x z) () x))))))
    (is (= 0 (count-occurrences 'w '((f x) y (((x z) x))))))
    ))

(deftest product-test
  (testing "(product sos1 sos2)"
    (is (= '((a x) (a y) (b x) (b y) (c x) (c y))) (product '(a b c) '(x y)))
    ))

(deftest filter-in-test
  (testing "(filter-in pred lst)"
    (is (= '(2 7) (filter-in number? '(a 2 (1 3) b 7))))
    (is (= '(a foo) (filter-in symbol? '(a (b c) 17 foo))))
    ))

(deftest list-index-test
  (testing "(list-index pre lst)"
    (is (= 1 (list-index number? '(a 2 (1 3) b 7))))
    (is (= 0 (list-index symbol? '(a 2 (1 3) b 7))))
    (is (= false (list-index symbol? '(1 2 (a b) 3))))
    ))

(deftest every-test
  (testing "(every? pred lst)"
    (is (= false (my-every? number? '(a b c 3 e))))
    (is (= true (my-every? number? '(1 2 3 4 5))))
    ))

(deftest exists-test
  (testing "(exists pred lst)"
    (is (= true (exists? number? '(a b c 3 e))))
    (is (= false (exists? number? '(a b c d e))))
    ))

(deftest up-test
  (testing "(up lst)"
    (is (= '(1 2 3 4) (up '((1 2) (3 4)))))
    (is (= '(x (y) z) (up '((x (y)) z))))
    ))

(deftest flatten-test
  (testing "(flatten slist)"
    (is (= '(a b c) (my-flatten '(a b c))))
    (is (= '(a b c) (my-flatten '((a) () (b ()) () (c)))))
    (is (= '(a b c) (my-flatten '(a b (( (c)))))))
    ))

(deftest merge-test
  (testing "(merge loi1 loi2)"
    (is (= '(1 1 2 4 8) (my-merge '(1 4) '(1 2 8))))
    (is (= '(3 35 62 81 83 85 90 90 91) (my-merge '(35 62 81 90 91) '(3 83 85 90))))
    ))

(deftest sort-test
  (testing "(sort loi)"
    (is (= '(2 2 3 5 8) (my-sort '(8 2 5 2 3))))))

(deftest sort-predicate-test
  (testing "sort pred loi"
    (is (= '(2 2 3 5 8) (my-sort->predicate < '(8 2 5 2 3))))))

(deftest bintree-test
  (testing "(bintree test)"
    (is (= {:key 5, :lson nil, :rson nil} (leaf 5)))
    (is (= 5 (contents-of (leaf 5))))
    (is (= true (leaf? (leaf 5))))
    (is (= {:key 'red, :lson {:key 5, :lson nil, :rson nil}, :rson {:key 8, :lson nil, :rson nil}} (interior-node 'red (leaf 5) (leaf 8))))
    (is (= 'red (contents-of (interior-node 'red (leaf 5) (leaf 8)))))
    (is (= {:key 5, :lson nil, :rson nil} (lson (interior-node 'red (leaf 5) (leaf 8)))))
    (is (= {:key 8, :lson nil, :rson nil} (rson (interior-node 'red (leaf 5) (leaf 8)))))
    (is (= false (leaf? (interior-node 'red (leaf 5) (leaf 8)))))
    ))

(deftest double-tree-test
  (testing "(double-tree tree)"
    (is (= 10 (contents-of (double-tree (leaf 5)))))))

(deftest mark-leaves-with-red-depth-test
  (testing "(mark-leaves-with-red-depth tree)"
    (is (= 2 (contents-of (rson (rson (rson (mark-leaves-with-red-depth
                                                (interior-node 'red
                                                (interior-node 'bar
                                                    (leaf 26)
                                                    (leaf 12))
                                                (interior-node 'red
                                                    (leaf 11)
                                                    (interior-node 'quux
                                                    (leaf 117)
                                                    (leaf 14)))))))))))))

(deftest path-test
  (testing "(path n tree)"
    (is (= '(right left left) (path 17 '(14 (7 () (12 () ()))
                                            (26 (20 (17 () ())
                                                    ())
                                                (31 () ()))))))))
