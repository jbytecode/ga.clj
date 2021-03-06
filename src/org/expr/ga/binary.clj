(ns org.expr.ga.binary)

(defn all-ones-cost [bits]
  (reduce + 0 bits))

(defn bits-to-long [bits]
  (long
   (reduce + 0
           (map #(*  %1 (Math/pow 2.0 %2)) (reverse bits) (range (count bits))))))

(defn long-to-bits [long-value]
  (vec (for [i (Long/toBinaryString long-value)] (Integer/valueOf (str i)))))

(defn bits-to-double [bits minvalue maxvalue]
  (let [n     (count bits)
        denom (dec (Math/pow 2.0 n))
        nom   (- maxvalue minvalue)] (* (bits-to-long bits) (+ minvalue (/ nom denom)))))

(defn create-chromosome [n]
  {:genes (take n (repeatedly #(rand-int 2)))
   :cost Double/MAX_VALUE})

(defn create-population [popsize chsize]
  (take popsize (repeatedly #(create-chromosome chsize))))

(defn calculate-costs-single-tread [population cost-fn]
  (for [ch population]
    (assoc ch :cost (cost-fn (:genes ch)))))

(defn calculate-costs [population cost-fn]
  (pmap
   #(assoc %1 :cost (cost-fn (:genes %1))) population))

(defn tournament-selection [pop]
  (->>
   (take 4 (repeatedly #(rand-nth pop)))
   (sort-by :cost)
   (take 2)))

(defn rank-selection [pop]
  (let
   [popsize       (count pop)
    ordered-pop   (sort-by :cost pop)
    ranks         (reverse (range 1 (inc popsize)))
    sum-ranks     (reduce + 0 ranks)
    probs         (map #(double (/ %1 sum-ranks)) ranks)
    cum-probs     (for [i (range 1 (inc popsize))] (reduce + 0 (take i probs)))
    ordered-pop2  (map #(assoc %1 :prob %2) ordered-pop cum-probs)
    [rand1 rand2] (take 2 (repeatedly rand))
    selected1     (first (filter #(< rand1 (:prob %1)) ordered-pop2))
    selected2     (first (filter #(< rand2 (:prob %1)) ordered-pop2))]
    [(dissoc selected1 :prob)
     (dissoc selected2 :prob)]))

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

(defn uniform-crossover
  ([chromosomes]
   (let
    [[f s] chromosomes
     genes1 (map #(if (< (rand) 0.5) %1 %2) (:genes f) (:genes s))
     genes2 (map #(if (< (rand) 0.5) %2 %1) (:genes f) (:genes s))]
     [{:genes (vec genes1) :cost Double/MAX_VALUE}
      {:genes (vec genes2) :cost Double/MAX_VALUE}]))
  ([ch1 ch2]
   (uniform-crossover [ch1 ch2])))

(defn mutation [prob ch]
  (let
   [flip       (fn [x] (if (zero? x) 1 0))
    new-genes (for [gene (:genes ch)] (if (< (rand) prob) (flip gene) gene))]
    {:genes new-genes :cost Double/MAX_VALUE}))

(defn generation [pop cost-fn selection-fn crossover-fn mutation-fn mutation-prob mapfn]
  (let
   [popsize         (count pop)
    popc            (calculate-costs pop cost-fn)
    new-gen         (first (for [_ (range popsize)] (->>
                                                     popc
                                                     (selection-fn)
                                                     (crossover-fn)
                                                     (mapfn #(mutation-fn mutation-prob %1)))))
    new-pop         (->>
                     (concat popc (calculate-costs new-gen cost-fn))
                     (sort-by :cost)
                     (take popsize))] new-pop))


(defn ga [& {:keys [popsize
                    chsize
                    cost-fn
                    selection-fn
                    mutation-fn
                    crossover-fn
                    mutation-prob
                    iters
                    mapfn] :or {popsize 20
                                selection-fn tournament-selection
                                mutation-fn mutation
                                crossover-fn one-point-crossover
                                mutation-prob 0.1
                                iters 100
                                mapfn pmap}}]
  (let [initial-population     (create-population popsize chsize)
        generation-fn  (fn [pop]
                         (generation
                          pop
                          cost-fn
                          selection-fn
                          crossover-fn
                          mutation-fn
                          mutation-prob
                          mapfn))
        last-population (last (take iters (iterate generation-fn initial-population)))
        best-chromosome (first (sort-by :cost last-population))
        best-solution   (:genes best-chromosome)
        best-cost       (:cost best-chromosome)]
    {:last-population last-population
     :best-chromosome best-chromosome
     :best-solution best-solution
     :best-cost best-cost}))
