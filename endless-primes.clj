(println "pls type n")
(def n (int (read)))

(def primes
  (letfn [(primes-chunks [p-chunk]
    (lazy-seq
      (when-let [[p & xs] (seq p-chunk)]
        (let [not-mult (fn [inp] (not(zero? (mod inp p))))]
          (cons p (primes-chunks (filter not-mult xs)))))))]
    (primes-chunks (iterate inc 2))))

(if (>= n 0)
  (println (nth primes n)))

; (if (>= n 0)
;   (println (take n primes)))
