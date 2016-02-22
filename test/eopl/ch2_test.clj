(ns eopl.ch2-test
  (:require [clojure.test :refer :all]
            [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]
            [eopl.ch2.05 :refer :all]
            [eopl.ch2.06 :refer :all]
            [eopl.ch2.07 :refer :all]
            [eopl.ch2.08 :refer :all]
            [eopl.ch2.09 :refer :all]
            [eopl.ch2.10 :refer :all]
            [eopl.ch2.11 :refer :all]
            [eopl.ch2.12 :refer :all]
            [eopl.ch2.13 :refer :all]
            [eopl.ch2.14 :refer :all]
            [eopl.ch2.15 :refer :all]
            [eopl.ch2.18 :refer :all]
  ))


(deftest env-test
  (testing "env"
    (is (= (apply-env-5 (e) 'd) 6))
    (is (= (apply-env-5 (e) 'y) 8))))

(deftest stack-test
  (testing "stack"
    (is (=  (top (push (empty-stack) 1)) 1))
    (is (=  (top (pop (x2))) 1))
    (is (=  (top (x2)) 2))
    (is (=  (top (x3)) 3))
    ))

(deftest env13-test
  (testing "13"
    (is (= (apply-env13 (e13) 'd) 6))
    (is (= (apply-env13 (e13) 'y) 8))
    (is (= (apply-env13 (e13) 'x) 7))
    (is (= (apply-env13 (e13) 'x) 7))
    (is (= (empty-env13? (empty-env13)) true))
    (is (= (empty-env13? (e13)) false))
    ))
