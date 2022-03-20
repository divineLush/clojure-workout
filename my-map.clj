(defn my-map
  ([f inp-coll] (my-map f inp-coll '[]))
  ([f inp-coll coll]
    (if (empty? inp-coll)
      coll
      (recur f (rest inp-coll) (conj coll (f (first inp-coll)))))))

(println (my-map (fn [x] (* x 2)) '[1 2 3 4 5]))
