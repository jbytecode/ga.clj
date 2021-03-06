(ns org.expr.ga.real-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.real :as r]))

(deftest test-random-normal
  (testing "Random values from normal distribution"

    (is
     (= "class java.util.Random"
        (str
         (class
          r/rng))))

    (let
     [r       (r/random-normal)]
      (is
       (< r 100))

      (is
       (> r -100)))

    (let
     [r       (r/random-normal :mean 100 :std 10)]
      (is
       (< r 200))

      (is
       (> r -200)))))



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


(deftest test-arithmetic-crossover
  (testing "Arithmetic crossover"
    (let
     [alpha              0.6
      inv-alpha          0.4
      min-range          [100 200 -300 0 -200]
      max-range          [500 300 500 100 -100]
      [ch1, ch2]         (take 2
                               (repeatedly
                                #(r/create-chromosome
                                  min-range
                                  max-range)))
      [off1 off2]          (r/arithmetic-crossover ch1 ch2
                                                   :alpha alpha)]

      (is
       (=
        (map #(+
               (* alpha %1)
               (* inv-alpha %2)) (:genes ch1) (:genes ch2))
        (:genes off1)))

      (is
       (=
        (map #(+
               (* alpha %1)
               (* inv-alpha %2)) (:genes ch2) (:genes ch1))
        (:genes off2))))))



(deftest test-random-normal-mutation
  (testing "Random normal mutation"
    (let
     [min-range          [100 200 -300 0 -200]
      max-range          [500 300 500 100 -100]
      ch                 (r/create-chromosome min-range max-range)
      mutated            (r/random-normal-mutation ch :mean 0 :std 1 :mutation-prob 1.0)
      min-bounds         (mapv #(- %1 100) min-range)
      max-bounds         (mapv #(+ %1 100) max-range)
      withins-range      (map #(and (>= %1 %2) (<= %1 %3))
                              (:genes mutated)
                              min-bounds
                              max-bounds)]

      (is
       (=
        (take 5 (repeat true))
        withins-range))

      (is
       (=
        (count (:genes mutated))
        5))

      (is
       (=
        (:cost mutated)
        Double/MAX_VALUE)))))





(deftest test-random-uniform-mutation
  (testing "Random uniform mutation"
    (let
     [min-range          [100 200 -300 0 -200]
      max-range          [500 300 500 100 -100]
      ch                 (r/create-chromosome min-range max-range)
      mutated            (r/random-uniform-mutation ch :lower -1 :upper 1 :mutation-prob 1.0)
      min-bounds         (mapv #(- %1 1) min-range)
      max-bounds         (mapv #(+ %1 1) max-range)
      withins-range      (map #(and (>= %1 %2) (<= %1 %3))
                              (:genes mutated)
                              min-bounds
                              max-bounds)]

      (is
       (=
        (take 5 (repeat true))
        withins-range))

      (is
       (=
        (count (:genes mutated))
        5))

      (is
       (=
        (:cost mutated)
        Double/MAX_VALUE)))))


(deftest test-create-population
  (testing "Create population of real chromosomes"
    (let
     [popsize      20
      lower        [0.0 0.0 0.0]
      upper        [10.0 10.0 10.0]
      pop          (r/create-population popsize lower upper)
      ch-top       (nth pop 0)
      withins-range      (map #(and (>= %1 %2) (<= %1 %3))
                              (:genes ch-top)
                              lower
                              upper)]
      (is
       (= (count pop) popsize))

      (is
       (=
        (take (count (:genes ch-top)) (repeat true))
        withins-range)))))