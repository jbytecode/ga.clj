(ns org.expr.ga.real)

(defn random-uniform [min-value max-value]
  (+ min-value (* (- max-value min-value) (rand))))

(defn create-chromosome [lower-vector upper-vector]
  {:genes (map #(random-uniform %1 %2) lower-vector upper-vector)
   :cost Double/MAX_VALUE})

