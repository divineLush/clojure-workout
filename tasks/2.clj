;task 2.1

(defn is-divisible-by? [n m]
  (if (integer? (/ n m))
    true
    false))

(defn sieve
  ([n]
   (sieve n 2 '(2) (range 3 (inc n))))
  ([n highest-prime primes numbers]
   (if (> highest-prime (Math/sqrt n))
     (concat primes numbers)
     (let [rest-numbers (remove
                               (fn [x]
                                 (and
                                   (is-divisible-by? x highest-prime)
                                   (not (= x highest-prime))))
                               numbers)]
      (recur n (first rest-numbers)
        (concat primes (list (first rest-numbers))) (rest rest-numbers))))))

(defn nth-prime [n] (nth (sieve (* n n)) (dec n)))

;task 2.2

(defn lazy-sieve
  ([highest-prime numbers]
   (let [rest-numbers (remove
                        (fn [x]
                          (is-divisible-by? x highest-prime))
                        numbers)]
     (lazy-seq (cons highest-prime (lazy-sieve (first rest-numbers) rest-numbers))))))

(def primes (lazy-sieve 2 (iterate inc 2)))
