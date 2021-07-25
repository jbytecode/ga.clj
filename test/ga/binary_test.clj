(ns ga.binary-test
  (:require [clojure.test :refer :all]
            [ga.binary :as b]))

(deftest test-create-chromosome
  (testing "Create binary chromosome"
    (let
     [ch (b/create-chromosome 10)]
      (is (= (count (:genes ch)) 10))
      (is (<= (reduce + 0 (:genes ch)) 10))
      (is (= (:cost ch) Double/MAX_VALUE)))))


(deftest test-create-population
  (testing "Create population"
    (let
     [pop     (b/create-population 20 5)
      top-ch  (first pop)]
      (is (= (count pop) 20))
      (is (= (:cost top-ch) Double/MAX_VALUE))
      (is (= (count (:genes top-ch)) 5)))))