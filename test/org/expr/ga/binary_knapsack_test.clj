(ns org.expr.ga.binary-knapsack-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.binary :as b]))


; Max z = 10x1 + 15x2 + 20x3 + 12x4 + 30x5 + 13x6 + 14x7
; Subject to:
; 5x1 + 6x2 + 7x3 + 4x4 + 3x5 + 2x6 + 8x7 <= 20
; where
; xi in {0, 1}
; The solution is x1 = x2 = x4 = x5 = x6 = 1
; x3 = x7 = 0
; Maximum of z is 80
(deftest test-binary-ga-knapsack-problem
  (testing "Knapsack Problem - binary ga"
    (let
     [objective    (fn [genes]
                     (let
                      [u           [10 15 20 12 30 13 14]
                       w           [5 6 7 4 3 2 8]
                       z           (reduce + 0 (map #(* %1 %2) u genes))
                       constraint  (reduce + 0 (map #(* %1 %2) w genes))]
                       (if (> constraint 20)
                         Double/MAX_VALUE
                         (* -1 z))))
      result           (b/ga
                        :popsize 50
                        :chsize 7
                        :iters 1000
                        :cost-fn objective)
      best-solution     (:best-solution result)
      best-cost         (:best-cost     result)]
      (is (= -80 best-cost))
      (is (= [1 1 0 1 1 1 0] best-solution)))))
