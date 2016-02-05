(ns eopl.inductive-sets-of-data-test
  (:require [clojure.test :refer :all]
            [eopl.inductive-sets-of-data :refer :all]))

(deftest in-S-test
  (testing "in-S?"
    (is (= true (in-S? 0)))
    (is (= true (in-S? 3)))
    (is (= false (in-S? 1)))
    (is (= false (in-S? 2)))
    ))

(deftest list-length-test
  (testing "list length"
    (is (= 3 (list-length '(a (b c) d))))
    (is (= 3 (list-length '(a b c))))
    (is (= 2 (list-length '((x) ()))))
    (is (= 2 (count '((x) ()))))
    ))

(deftest nth-element-test
  (testing "nth element"
    (is (= 'b (nth '(a b c) 1)))
    (is (= 'b (nth-element '(a b c) 1)))
    ))

(deftest remove-first-test
  (testing "remove first"
    (is (= '(b c) (remove-first 'a '(a b c))))
    (is (= '(e f g) (remove-first 'b '(e f g))))
    (is (= '(c1 c1 a4) (remove-first 'a4 '(c1 a4 c1 a4))))
    (is (= '() (remove-first 'x '())))
    ))

(deftest occurs-free?_test
  (testing "occurs free?"
    (is (= true  (occurs-free? 'x 'x)))
    (is (= false (occurs-free? 'x 'y)))
    (is (= false (occurs-free? 'x '(lambda (x) (x y)))))
    (is (= true  (occurs-free? 'x '(lambda (y) (x y)))))
    (is (= true  (occurs-free? 'x '((lambda (x) x) (x y)))))
    (is (= true  (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))))
    ))

(deftest subst-test
  (testing "subst test"
    (is (= '(a a c d e) (subst 'a 'b '(a b c d e))))
    (is (= '(a) (subst 'a 'b '(b))))
    (is (= '(a a a) (subst 'a 'b '(b b b))))
    (is (= '((s b) c d s) (subst 's 'a '((a b) c d s))))
    ))

(deftest number-elements-test
  (testing "number elements"
    (is (= (number-elements '(a b c d e)) '((0 a) (1 b) (2 c) (3 d) (4 e))))))

(deftest list-sum-test
  (testing "(list-sum (0 (1 (2)))"
    (is (= (list-sum (list 1 2 3 4 5)) 15))))