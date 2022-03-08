(def input-str (str (read)))

(def st
  (distinct
    (clojure.string/split input-str #"")))

(def st-len (count st))

(defn filter-st [x]
  (filter
     (fn [el] (not (= (str x) el)))
     st))

(defn gen-strs [x]
  (map
     (fn [el]
       (str x el))
     (filter-st (last x))))

; (loop [i 0]
;   (when (< i st-len)
;     (println (gen-strs (nth st i)))
;     (recur (inc i))))

(defn gen-seq-for-str [x]
  (flatten
    (map
      (fn [el]
        (gen-strs el))
      x)))

(println (gen-seq-for-str st))
