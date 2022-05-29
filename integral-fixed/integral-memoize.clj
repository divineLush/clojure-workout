; step = x / n
(def step 0.01)


(def calc-sum
  (memoize (fn [i f]
    (if (zero? i)
      (/ (f 0) 2)
      (+ (f (* step i)) (calc-sum (dec i) f))))))


(defn calc-integral [f]
  (fn [x]
    (let [n (int (/ x step))]
      (* step (+ (calc-sum n f) (/ (f x) 2))))))


(defn test-func [x] (* x x))
(defn real-res [x] (float (/ (* x x x) 3)))

(println ((calc-integral test-func) 2))
(println (real-res 2))

(println ((calc-integral test-func) 3))
(println (real-res 3))

(println ((calc-integral test-func) 4))
(println (real-res 4))
