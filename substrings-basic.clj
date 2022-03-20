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

(defn build-seq
  ([msg] (build-seq msg '[] alphabet))
  ([msg cur-seq alph-seq]
    (if (empty? alph-seq)
      cur-seq
      (recur msg (smart-conj cur-seq msg (first alph-seq)) (rest alph-seq)))))

(defn concat-seqs
  ([inp-seq] (concat-seqs inp-seq '[]))
  ([inp-seq cur-seq]
    (if (empty? inp-seq)
      cur-seq
      (recur (rest inp-seq) (concat cur-seq (build-seq (first inp-seq)))))))

(defn gen-seq
  ([] (gen-seq n alphabet))
  ([i cur-seq]
    (if (= i 1)
      cur-seq
      (recur (dec i) (concat-seqs cur-seq)))))

(if (> n 1)
  (println (gen-seq)))
