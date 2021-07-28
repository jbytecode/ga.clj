(ns org.expr.ga.expression)

(def third  (comp second rest))

(defn sqrt [x]
  (if (< x 0) 0 (Math/sqrt x)))

(defn div [x y]
  (if (zero? y) 0 (/ x y)))

(def functions
  '({:func + :p 2}
    {:func - :p 2}
    {:func * :p 2}
    {:func div :p 2}
    {:func sqrt :p 1}))

(def constants (range 10))

(defn random-expression [functions constants]
  (let
   [func      (rand-nth functions)
    p         (:p func)
    fname     (:func func)
    r         (rand)]
    (cond
      (< r 0.5) (rand-nth constants)
      (< r 1.0) (cons fname
                      (for [_ (range p)]
                        (random-expression functions constants))))))

(defn random-subset [expr]
  (let
   [c           (if (seq? expr) (count expr) 0)]
    (cond
      (= c 0)            expr
      (= c 2)            (if (< (rand) 0.5) (random-subset (second expr)) expr)
      (= c 3)            (let [p (rand)]
                           (cond
                             (< p 1/4) (random-subset (second expr))
                             (< p 2/4) (random-subset (third expr))
                             (< p 4/4) expr)))))



