(ns org.expr.ga.real-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.real :as r]))


(deftest test-random-uniform
  (testing "Random uniform using ranges"
    (let
     [min-range          [100 200 -300 0 -200]
      max-range          [500 300 500 100 -100]
      random-numbers     (map #(r/random-uniform %1 %2) min-range max-range)
      withins-range      (map #(and (>= %1 %2) (<= %1 %3))
                              random-numbers
                              min-range
                              max-range)]
      (is
       (=
        (take 5 (repeat true))
        withins-range)))))


(deftest test-create-chromosome
  (testing "Create real chromosome within range"
    (let
     [min-range          [100 200 -300 0 -200]
      max-range          [500 300 500 100 -100]
      ch                 (r/create-chromosome min-range max-range)
      genes              (:genes ch)
      withins-range      (map #(and (>= %1 %2) (<= %1 %3))
                              genes
                              min-range
                              max-range)]
      (is
       (=
        (take 5 (repeat true))
        withins-range))

      (is
       (=
        (:cost ch)
        Double/MAX_VALUE))

      (is
       (=
        (count genes)
        (count min-range)
        (count max-range))))))