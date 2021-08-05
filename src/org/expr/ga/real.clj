(ns org.expr.ga.real
  (:import [java.util Random]))

(defonce rng (Random.))

(defn random-uniform [min-value max-value]
  (+ min-value (* (- max-value min-value) (rand))))

(defn random-normal [& {:keys [mean std] :or {mean 0 std 1}}]
  (+ mean (* std (.nextGaussian rng))))

(defn create-chromosome [lower-vector upper-vector]
  {:genes (map #(random-uniform %1 %2) lower-vector upper-vector)
   :cost Double/MAX_VALUE})

(defn arithmetic-crossover [ch1 ch2 & {:keys [alpha] :or {alpha 0.5}}]
  (let
   [inv-alpha (- 1.0 alpha)
    genes1    (mapv #(+ (* alpha %1) (* inv-alpha %2))
                    (:genes ch1)
                    (:genes ch2))
    genes2    (mapv #(+ (* alpha %2) (* inv-alpha %1))
                    (:genes ch1)
                    (:genes ch2))]
    [{:genes genes1 :cost Double/MAX_VALUE}
     {:genes genes2 :cost Double/MAX_VALUE}]))

(defn random-mutation [ch & {:keys
                             [mutation-prob mean std]
                             :or
                             {mutation-prob 0.10
                              mean 0
                              std 1}}]
  {:genes (mapv #(if
                  (< (rand) mutation-prob)
                   (+ %1 (random-normal :mean mean :std std))
                   %1) (:genes ch))
   :cost Double/MAX_VALUE})