(ns eopl.ch2-test
  (:require [clojure.test :refer :all]
            [eopl.common :refer :all]
            [eopl.data-abstraction :refer :all]
            [eopl.ch2.05 :refer :all]
  ))


(deftest env-test
  (testing "env"
    (is (= (apply-env-5 (e) 'd) 6))
    (is (= (apply-env-5 (e) 'y) 8))))
