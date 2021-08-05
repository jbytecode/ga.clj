(ns org.expr.ga.compact)

(defn sample
  "Generates a vector of 1 and 0s using a probability vector <br>
   **Example**: <br>
   `(sample [0.5 0.5])`  => `[0 1]` <br>
   `(sample [1.0 1.0])`  => `[1 1]` <br>
   `(sample [0.0 0.0])`  => `[0 0]` <br>
   "
  [prob-vector]
  (map #(if (< (rand) %1) 1 0) prob-vector))





(defn init-prob-vector
  "Generates an initial vector of probabilities <br>
  **Example**: <br>
   `(init-prob-vector 5)`  =>  `[0.5 0.5 0.5 0.5 0.5]` <br> 
   "
  [chsize]
  (take chsize (repeat 0.5)))





(defn update-fn
  "
   Update a probability using a bit of winner and loser.
   If the winner equals to loser, does nothing.
   If the winner is 1, then update the prob using
   prob := prob + 1 / popsize
   otherwise
   prob := prob - 1 / popsize
  "
  [winner-bit loser-bit prob popsize]
  (if (= winner-bit loser-bit)
    prob
    (if (= winner-bit 1)
      (+ prob (/ 1.0 popsize))
      (- prob (/ 1.0 popsize)))))







(defn update-prob-vector [winner loser prob-vector popsize]
  (mapv #(update-fn %1 %2 %3 popsize) winner loser prob-vector))

(defn cga-single-step [cost-fn prob-vector popsize]
  (let
   [[ch1 ch2]     (pvalues (sample prob-vector) (sample prob-vector))
    [cost1 cost2] (pvalues (cost-fn ch1) (cost-fn  ch2))
    winner        (if (< cost1 cost2) ch1 ch2)
    loser         (if (< cost1 cost2) ch2 ch1)]
    (update-prob-vector winner loser prob-vector popsize)))

(defn stops? [prob-vector & {:keys [eps] :or {eps 0.001}}]
  (every? #(or
            (< (Math/abs (- %1 1)) eps)
            (< (Math/abs (- %1 0)) eps)) prob-vector))

(defn cga [popsize chsize cost-fn & {:keys [eps] :or {eps 0.001}}]
  (loop [prob-vector (init-prob-vector chsize)]
    (if (stops? prob-vector :eps eps)
      (map int prob-vector)
      (recur (cga-single-step cost-fn prob-vector popsize)))))