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

(defn tournament-selection [pop]
  (->>
   (take 4 (repeatedly #(rand-nth pop)))
   (sort-by :cost)
   (take 2)))


(defn one-point-crossover
  ([chromosomes]
   (let [f          (first chromosomes)
         s          (second chromosomes)
         n-genes    (count (:genes f))
         cut-point  (rand-int n-genes)
         f-new      (map #(if (< %1 cut-point) %2 %3) (range n-genes) (:genes f) (:genes s))
         s-new      (map #(if (< %1 cut-point) %3 %2) (range n-genes) (:genes f) (:genes s))]
     [{:genes f-new :cost Double/MAX_VALUE}
      {:genes s-new :cost Double/MAX_VALUE}]))

  ([ch1 ch2]
   (one-point-crossover [ch1 ch2])))

(defn mutation [prob ch]
  (let
   [flip       (fn [x] (if (zero? x) 1 0))
    new-genes (for [gene (:genes ch)] (if (< (rand) prob) (flip gene) gene))]
    {:genes new-genes :cost Double/MAX_VALUE}))