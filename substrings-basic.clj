(println "pls type smth")
(def alphabet (str (read)))

(println "pls input n")
(def n (int (read)))

(defn is-last-equal [x y]
  (= (str (last (str x))) (str y)))

(defn smart-conj [s x y]
  (if (is-last-equal x y)
    s
    (conj s (str x y))))

(defn build-coll
  ([msg] (build-coll msg '[] alphabet))
  ([msg coll alph-seq]
    (if (empty? alph-seq)
      coll
      (recur msg (smart-conj coll msg (first alph-seq)) (rest alph-seq)))))

(defn concat-colls
  ([inp-coll] (concat-colls inp-coll '[]))
  ([inp-coll coll]
    (if (empty? inp-coll)
      coll
      (recur (rest inp-coll) (concat coll (build-coll (first inp-coll)))))))

(defn gen-coll
  ([] (gen-coll n alphabet))
  ([i coll]
    (if (= i 1)
      coll
      (recur (dec i) (concat-colls coll)))))

(if (> n 1)
  (println (gen-coll)))
