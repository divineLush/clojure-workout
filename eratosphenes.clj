(def n (int (read)))
(def nums (range 2 (inc n)))

(defn notMult [nm dv]
  (not (zero? (mod nm dv))))

(defn sieve [ls nm]
  (filter
    (fn [el] (notMult el nm))
    ls))

(defn eratosphenes [candidates]
  (if (not (empty? candidates))
    (let [curPrime (first candidates)]
      (println curPrime)
      (recur (sieve candidates curPrime)))))

(eratosphenes nums)
