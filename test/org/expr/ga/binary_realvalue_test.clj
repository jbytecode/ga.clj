(ns org.expr.ga.binary-realvalue-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.binary :as b])
)


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

