(println "pls type n")
(def n (int (read)))

(def primes
  (letfn [(primes-parts [part]
    (lazy-seq
      (when-let [[p & xs] (seq part)]
        (let [cant-div #(not(zero? (mod % p)))]
          (cons p (primes-parts (filter cant-div xs)))))))]
            (primes-parts (iterate inc 2))))

(println (nth primes n))
