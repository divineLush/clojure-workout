(def step 0.01)


(defn partial-sol
  ([f] (partial-sol f (/ (f 0) 2) 1))
  ([f val i] (def new-val (+ (f (* step i)) val))
    (lazy-seq (cons new-val (partial-sol f new-val (inc i))))))


(defn calc-integral [f]
  (let [part (partial-sol f)]
    (fn [x]
      (* step (+ (nth part (dec (int (/ x step)))) (/ (f x) 2))))))


(defn test-func [x] (Thread/sleep 1) (* x x))
(defn real-res [x] (float (/ (* x x x) 3)))
(def res (calc-integral test-func))

(println (res 2))
(println (real-res 2))

(println '><><><><><><><><><><><)

(println (res 3))
(println (real-res 3))

(println '><><><><><><><><><><><)

(println (res 4))
(println (real-res 4))

(println '><><><><><><><><><><><)

(time (res 1))
(time (res 2))
