(defn my-map [f coll]
  (reduce
    (fn [a b] (conj a (f b)))
    []
    coll))

(println (my-map (fn [x] (* x 2)) '[1 2 3 4 5]))
