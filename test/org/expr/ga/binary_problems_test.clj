(ns org.expr.ga.binary-problems-test
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


; Min z = |x1 - PI| + |x2 - exp(1)|
; where
; 0 <= x1, x2 <= 10
(deftest test-real-optimization-with-binary-genes
  (testing "Real-valued optimization with binary genes"
    (let
     [bit-len        30
      cost-fn        (fn [bits]
                       (let
                        [bit-parts        (partition bit-len bits)
                         [value1 value2]  (map #(b/bits-to-double %1 0.0 10.0) bit-parts)
                         result           (+ (Math/abs (- value1 Math/PI)) (Math/abs (- value2 Math/E)))] result))
      result           (b/ga
                        :popsize 100
                        :chsize (* bit-len 2)
                        :iters 500
                        :cost-fn cost-fn)
      best-solution     (:best-solution result)
      best-cost         (:best-cost     result)
      paired-bits       (partition bit-len best-solution)
      values            (map #(b/bits-to-double %1 0.0 10.0) paired-bits)]
      (is
       (< best-cost 0.05))
      (is
       (< (Math/abs (- (first values) 3.14)) 0.05))
      (is
       (< (Math/abs (- (second values) 2.71)) 0.05)))))
