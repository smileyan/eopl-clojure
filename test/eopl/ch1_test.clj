(ns eopl.ch1-test
  (:require [clojure.test :refer :all]
            [eopl.ch1.07 :refer :all]
            [eopl.ch1.08 :refer :all]
            [eopl.ch1.09 :refer :all]))

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
