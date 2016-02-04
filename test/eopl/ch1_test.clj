(ns eopl.ch1-test
  (:require [clojure.test :refer :all]
            [eopl.common :refer :all]
            [eopl.ch1.07 :refer :all]
            [eopl.ch1.08 :refer :all]
            [eopl.ch1.09 :refer :all]
            [eopl.ch1.12 :refer :all]
            [eopl.ch1.13 :refer :all]
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
