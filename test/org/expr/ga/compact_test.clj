(ns org.expr.ga.compact-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.binary :as b]
            [org.expr.ga.compact :as c]))

(deftest test-sample
  (testing "Sampling chromosome using vector of probabilities"
    (is (= (c/sample [1.0 1.0 1.0]) [1 1 1]))
    (is (= (c/sample [0.0 0.0 0.0]) [0 0 0]))
    (let
     [s   (c/sample [0.5 0.5 0.5])]
      (is
       (or
        (= s [0 0 0])
        (= s [0 0 1])
        (= s [0 1 0])
        (= s [1 0 0])
        (= s [1 1 1])
        (= s [1 1 0])
        (= s [0 1 1])
        (= s [1 0 1]))))))


(deftest test-update-prob-vector
  (testing "Updating vector of probabilities"
    (is
     (=
      (c/update-prob-vector [1 1 1] [0 0 0] [0.5 0.5 0.5] 10)
      [0.6 0.6 0.6]))
    (is
     (=
      (c/update-prob-vector [0 0 0] [1 1 1] [0.5 0.5 0.5] 10)
      [0.4 0.4 0.4]))
    (is
     (=
      (c/update-prob-vector [1 1 0] [0 0 0] [0.5 0.5 0.5] 10)
      [0.6 0.6 0.5]))
    ))

(deftest test-stop-cga
  (testing "Stopping criterion of cga"
    (is (c/stops? [1.0 1.0 1.0]))
    (is (c/stops? [0.0 0.0 0.0]))
    )
  )


(deftest test-cga 
  (testing "CGA - all ones test"
    (is (c/cga 100 8 b/all-ones-cost) [0 0 0 0 0 0 0 0])
    (is (c/cga 100 8 (fn [bits] (* -1 (b/all-ones-cost bits)))) [1 1 1 1 1 1 1 1])
    ))