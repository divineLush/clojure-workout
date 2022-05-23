(println "pls type x")
(def x (int (read)))

(defn input-fn [a] (/ 1 (Math/log a)))

(def steps-num 15)
(def step (float (/ x steps-num)))

(def points (range 0 x step))
(def values (map input-fn points))

(def partial-sum
  (reduce + (rest (drop-last values))))

(def first-and-last
  (float (/ (+ (first values) (last values)) 2)))

(def res
  (* step (+ first-and-last partial-sum)))

(println res)
