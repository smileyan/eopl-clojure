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