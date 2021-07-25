(ns ga.binary-test
  (:require [clojure.test :refer :all]
            [ga.binary :as b]))

(deftest test-bits-to-long
  (testing "Bits to Long"
    (is (= (b/bits-to-long [1 1 1]) 7))
    (is (= (b/bits-to-long [1 1 1 1]) 15))
    (is (= (b/bits-to-long [1 1 0 0]) 12))))

(deftest test-bits-to-double
  (testing "Bits to double"
    (is (= (b/bits-to-double [0 0 0] 0.0 1.0) 0.0))
    (is (= (b/bits-to-double [1 1 1] 0.0 1.0) 1.0))
    (is (= (b/bits-to-double [0 0 1 1] 0.0 1.0) 0.2))))

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

(deftest test-calculate-costs
  (testing "Calculate costs"
    (let
     [pop (b/calculate-costs (b/create-population 20 10) b/all-ones-cost)]
      (doseq [ch pop]
        (is (= (reduce + 0 (:genes ch)) (:cost ch)))))))

(deftest test-tournament-selection
  (testing "Tournament selection (never selects the worst)"
    (let [pop             (b/create-population 20 10)
          pop-with-costs  (b/calculate-costs pop b/all-ones-cost)
          worst           (->
                           (sort-by :cost pop-with-costs)
                           last)
          [f s]           (b/tournament-selection pop-with-costs)]
      (is (>= (:cost worst) (:cost f)))
      (is (>= (:cost worst) (:cost s))))))


(deftest test-one-point-chrossover
  (testing "One-point cross-over"
    (let [f           {:genes [1 1 1] :cost 3}
          s           {:genes [0 0 0] :cost 0}
          [fnew snew] (b/one-point-crossover f s)]
      (is (= (count (:genes fnew)) 3))
      (is (= (count (:genes snew)) 3))
      (is (= (:cost fnew) Double/MAX_VALUE))
      (is (= (:cost snew) Double/MAX_VALUE))
      (is (or
           (= (:genes fnew) [0 0 0])
           (= (:genes fnew) [1 0 0])
           (= (:genes fnew) [1 1 0])
           (= (:genes fnew) [1 1 1])))
      (is (or
           (= (:genes snew) [0 0 0])
           (= (:genes snew) [0 1 1])
           (= (:genes snew) [0 0 1])
           (= (:genes snew) [1 1 1]))))))

(deftest test-mutation
  (testing "Binary mutation with some probability"
    (let
     [ch            {:genes [1 1 1] :cost 3}
      mutated1      (b/mutation 1.0 ch)
      mutated2      (b/mutation 0.5 ch)]
      (is (:genes mutated1) [0 0 0])
      (is
       (or
        (= (:genes mutated2) [1 1 1])
        (= (:genes mutated2) [1 1 0])
        (= (:genes mutated2) [1 0 0])
        (= (:genes mutated2) [0 0 0])
        (= (:genes mutated2) [1 0 1])
        (= (:genes mutated2) [0 1 1])
        (= (:genes mutated2) [0 1 0])
        (= (:genes mutated2) [0 0 1]))))))


(deftest test-binary-ga-all-ones-example
  (testing "All genes are ones - example - binary ga"
    (let
     [all-ones-cost    (fn [genes] (reduce + 0 genes))
      result           (b/ga
                        :popsize 50
                        :chsize 10
                        :iters 500
                        :cost-fn all-ones-cost)
      best-solution     (:best-solution result)
      best-cost         (:best-cost     result)]
      (is (zero? best-cost))
      (is (= (zero? (reduce + 0 best-solution)))))))