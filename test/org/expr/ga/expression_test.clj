(ns org.expr.ga.expression-test
  (:require [clojure.test :refer :all]
            [org.expr.ga.expression :as e]))

(deftest test-random-subset
  (testing "Random subset of expression"
    (let
     [expr          '(+ (* 5 10) (- 8 4))
      sexpr         (e/random-subset expr)]
      (is
       (or
        (= sexpr 5)
        (= sexpr 10)
        (= sexpr 8)
        (= sexpr 4)
        (= sexpr '(- 8 4))
        (= sexpr '(* 5 10))
        (= sexpr expr))))))