; (println "pls type x")
; (def input-x (int (read)))

(defn sum [f x n]
  (let [step (/ x n)
        points (map f (range 0 x step))
        sum-seq (reduce + (rest (drop-last points)))
        first-and-last (/ (+ (first points) (last points)) 2)]
   (* step (+ sum-seq first-and-last))))

(defn func [x] (* 2 x))

(println (sum func 10 10))
(println (sum func 20 20))
(println (sum func 30 30))
