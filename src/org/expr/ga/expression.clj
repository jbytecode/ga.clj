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


(defn find-function-by-name [name functions]
  (first (filter #(= name (:func %1)) functions)))

(defn arity-by-name [name functions]
  (:p (find-function-by-name name functions)))

(defn functions-of-arity [arity functions]
  (filter #(= arity (:p %1)) functions))


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



(defn mutate-fn-with [expr functions]
  (let
   [op                 (first expr)
    op-fn-entry        (find-function-by-name op functions)
    arity              (:p op-fn-entry)
    potential-fns      (functions-of-arity arity functions)
    lucky-fn           (rand-nth potential-fns)
    lucky-fn-op        (:func lucky-fn)
    new-expr           (cons lucky-fn-op (for [e (rest expr)] e))] new-expr))

(defn mutate-args-with [expr functions constants]
  (let
   [r           (rand)]
    (cond
      (not (seq? expr))   (rand-nth constants)
      (< r 0.50)   expr
      (< r 1.0)   (cons (first expr)
                        (for [e (rest expr)]
                          (mutate-args-with
                           (random-expression functions constants) functions constants))))))

(defn mutate-with [expr functions constants]
  (if (< (rand) 0.5)
    (mutate-fn-with expr functions)
    (mutate-args-with expr functions constants)))

