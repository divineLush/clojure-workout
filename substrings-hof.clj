(println "pls type smth")
(def alphabet (str (read)))

(println "pls input n")
(def n (int (read)))

(def st
  (distinct
    (clojure.string/split alphabet #"")))

(defn filter-st [x]
  (filter
    (fn [el] (not (= (str x) el)))
    st))

(defn gen-strs [x]
  (map
    (fn [el]
      (str x el))
    (filter-st (last x))))

(defn gen-seq-for-str [x]
  (flatten
    (map
      (fn [el]
        (gen-strs el))
      x)))

(defn gen-seq-req [sq i]
  (if (< i n)
    (let [cur-seq (gen-seq-for-str sq)]
      (println cur-seq)
      (recur cur-seq (inc i)))))

(gen-seq-req st 1)
