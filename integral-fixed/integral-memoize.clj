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


(def calc-sum-slow
  (fn [i f]
    (if (zero? i)
      (/ (f 0) 2)
      (+ (f (* step i)) (calc-sum-slow (dec i) f)))))


(defn calc-integral-slow [f]
  (fn [x]
    (let [n (int (/ x step))]
      (* step (+ (calc-sum-slow n f) (/ (f x) 2))))))


(println 'memoized)
(println (time (mem-res 2)))
(println 'slow)
(def slow-res (calc-integral-slow test-func))
(println (time (slow-res 2)))

(println '><><><><><><><><><><><)

(println (time (mem-res 2.3)))
(println (time (slow-res 2.3)))

(println '><><><><><><><><><><><)

(println (time (mem-res 2.6)))
(println (time (slow-res 2.6)))

(println '><><><><><><><><><><><)

(println (mem-res 3.3))
(println (real-res 3.3))

(println '><><><><><><><><><><><)

(println (mem-res 2.1))
(println (real-res 2.1))

(println '><><><><><><><><><><><)

(println (mem-res 1.6))
(println (real-res 1.6))
