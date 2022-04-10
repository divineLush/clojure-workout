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

(defn flatten-custom [coll]
  (reduce
    (fn [a b]
      (concat a b))
    []
    coll))

(defn gen-seq-for-str [x]
  (flatten-custom
    (map
      (fn [el]
        (gen-strs el))
      x)))

(println (nth (iterate gen-seq-for-str st) (dec n)))
