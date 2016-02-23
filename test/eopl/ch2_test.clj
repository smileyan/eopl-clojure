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
            [eopl.ch2.19 :refer :all]
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

(deftest ch2-18-test
  (testing "ch2-18"
    (is (= (number->sequence 7) '(7 () ())))
    (is (= (current-element '(6 (5 4 3 2 1) (7 8 9))) 6))
    (is (= (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9))))
    (is (= (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9))))
    (is (= (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9))))
    (is (= (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9))))
    ))

(deftest ch2-19-test
  (testing "ch2-19"
    (is (= (number->bintree 13) '(13 () ())))
    (is (= (t1) '(13 (12 () ()) (14 () ()))))
    (is (= (number->bintree 13) '(13 () ())))
    (is (= (move-to-left-son (t1)) '(12 () ())))
    (is (= (current-element-bt (move-to-left-son (t1))) 12))
    (is (= (at-leaf? (move-to-right-son (move-to-left-son (t1)))) true))
    (is (= (insert-to-left-bt 15 (t1)) '(13 (15 (12 () ()) ()) (14 () ()))))
    ))