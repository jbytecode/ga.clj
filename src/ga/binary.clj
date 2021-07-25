(ns ga.binary)

(defn all-ones-cost [bits]
  (reduce + 0 bits))

(defn create-chromosome [n]
  {:genes (take n (repeatedly #(rand-int 2)))
   :cost Double/MAX_VALUE})

(defn create-population [popsize chsize]
  (take popsize (repeatedly #(create-chromosome chsize))))

(defn calculate-costs [population cost-fn]
  (for [ch population]
    (assoc ch :cost (cost-fn (:genes ch)))))