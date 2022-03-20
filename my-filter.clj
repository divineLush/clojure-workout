(defn my-filter
  ([f inp-coll] (my-filter f inp-coll '[]))
  ([f inp-coll coll]
    (if (empty? inp-coll)
      coll
      (recur
        f
        (rest inp-coll)
        (if (f (first inp-coll))
          (conj coll (first inp-coll))
          coll)))))

(println (my-filter even? '[1 2 3 4 5]))
