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


(deftest test-find-function-by-name
  (testing "Find function by name"
    (is (=  {:func 'div :p 2} (e/find-function-by-name 'div e/functions)))
    (is (=  {:func 'sqrt :p 1} (e/find-function-by-name 'sqrt e/functions)))
    (is (=  {:func '+ :p 2} (e/find-function-by-name '+ e/functions)))))

(deftest test-arity-by-name
  (testing "Arity by name"
    (is (=  2 (e/arity-by-name '+ e/functions)))
    (is (=  2 (e/arity-by-name 'div e/functions)))
    (is (=  1 (e/arity-by-name 'sqrt e/functions)))))

(deftest test-functions-of-arity
  (testing "Functions of arity"
    (let
     [fns1            (e/functions-of-arity 1 e/functions)
      fns2            (e/functions-of-arity 2 e/functions)]
      (is (some #(= (:func %1) '+) fns2))
      (is (some #(= (:func %1) '-) fns2))
      (is (some #(= (:func %1) 'sqrt) fns1)))))