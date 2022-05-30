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


(defn test-func [x] (Thread/sleep 1) (* x x))
(defn real-res [x] (float (/ (* x x x) 3)))

(def mem-res (calc-integral test-func))

(println (mem-res 2))
(println (real-res 2))

(println '><><><><><><><><><><><)

(println (mem-res 3))
(println (real-res 3))

(println '><><><><><><><><><><><)

(println (mem-res 4))
(println (real-res 4))


(def calc-sum-slow
  (fn [i f]
    (if (zero? i)
      (/ (f 0) 2)
      (+ (f (* step i)) (calc-sum-slow (dec i) f)))))


(defn calc-integral-slow [f]
  (fn [x]
    (let [n (int (/ x step))]
      (* step (+ (calc-sum-slow n f) (/ (f x) 2))))))

(println '><><><><><><><><><><><)

(println 'memoized)
(time (mem-res 2))
(time (mem-res 3))

(println 'slow)
(def slow-res (calc-integral-slow test-func))
(time (slow-res 2))
(time (slow-res 3))
