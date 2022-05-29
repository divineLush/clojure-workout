(def h 0.01)

;task 3.1

(def eval-integral-rec-mem1
  (memoize (fn [i f]
             (if (= 0 i)
               (/ (f 0) 2)
               (+ (f (* h i)) (eval-integral-rec-mem1 (dec i) f))))))

(defn eval-integral-rec-mem [f]
  (fn [x]
    (if (integer? (int (/ x h)))
      (* h (+ (eval-integral-rec-mem1 (dec (int (/ x h))) f) (/ (f x) 2)))
      (* h (+ (eval-integral-rec-mem1 (int (/ x h)) f) (/ (f x) 2))))))

;task 3.2

(defn partial-solutions
  ([f] (partial-solutions f (/ (f 0) 2) 1))
  ([f val i] (def new-val (+ (f (* h i)) val))
              (lazy-seq (cons new-val (partial-solutions f new-val (inc i))))))

(defn eval-integral-partial1 [f]
  (let [ev (partial-solutions f)]
    (fn [x]
      (if (integer? (int (/ x h)))
      (* h (+ (nth ev (- (int (/ x h)) 2)) (/ (f x) 2)))
      (* h (+ (nth ev (dec (int (/ x h)))) (/ (f x) 2)))))))

;misc

(defn func1 [x] (Thread/sleep 1) (/ 7 (+ (* x x) 1)))

(defn func2 [x] x)

(defn func3 [x] 10)

(let [integral (eval-integral-partial func1)]
  (time (integral 1))
  (time (integral 2))
  (time (integral 2.1))
  (time (integral 2.5))
  (time (integral 2.56789))
  (time (integral 3)))
