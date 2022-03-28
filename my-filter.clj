(defn my-filter [f coll]
  (reduce
    (fn [a b]
      (if (f b) (conj a b) a))
    []
    coll))

(println (my-filter even? '[1 2 3 4 5]))
