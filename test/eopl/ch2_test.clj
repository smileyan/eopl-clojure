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
  ))


(deftest env-test
  (testing "env"
    (is (= (apply-env-5 (e) 'd) 6))
    (is (= (apply-env-5 (e) 'y) 8))))
