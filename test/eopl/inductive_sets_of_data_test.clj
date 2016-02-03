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
