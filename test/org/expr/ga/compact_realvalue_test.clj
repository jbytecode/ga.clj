(ns org.expr.ga.compact-realvalue-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.binary :as b]
            [org.expr.ga.compact :as c]))


; Min z = |x1 - PI| + |x2 - exp(1)|
; where
; 0 <= x1, x2 <= 10
(deftest test-real-optimization-using-compact-ga
  (testing "Real-valued optimization using compact genetic algorithms"
    (let
     [bit-len        30
      cost-fn        (fn [bits]
                       (let
                        [bit-parts        (partition bit-len bits)
                         [value1 value2]  (map #(b/bits-to-double %1 0.0 10.0) bit-parts)]
                         (+
                          (Math/abs (- value1 Math/PI))
                          (Math/abs (- value2 Math/E)))))
      result            (c/cga 1000 (* bit-len 2) cost-fn)
      best-cost         (cost-fn  result)
      paired-bits       (partition bit-len result)
      values            (map #(b/bits-to-double %1 0.0 10.0) paired-bits)]
      (is
       (< best-cost 0.05))
      (is
       (< (Math/abs (- (first values) 3.14)) 0.05))
      (is
       (< (Math/abs (- (second values) 2.71)) 0.05)))))

