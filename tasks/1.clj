(use 'clojure.string)

;tasks 1.1 & 1.2

(defn n-str-No-adj-rec
  ([lang n]
   (if (= n 1)
     lang
     (n-str-no-adj-rec lang n 0 lang)))
  ([lang n i res]
   (cond
     (= (count (last res)) n) res
     (= i (count lang)) (recur lang n 0 (take (- (count res) 1) res))
     true (if
            (ends-with? (last res) (nth lang i))
            (recur lang n (inc i) res)
            (recur lang n (inc i) (cons (.concat (last res) (nth lang i)) res))))))

;task 1.3

(defn my-map
  [f coll]
  (reduce (fn [l x] (cons (f x) l)) '() (reverse coll)))

(defn my-filter [pred coll]
  (reduce (fn [l x] (if (pred x)
                      (cons x l)))
          '() (reverse coll)))

;task 1.4

(defn my-concat1 [lang s]
  (map (fn [x] (.concat s x)) (filter (fn [x] (not (ends-with? s x))) lang)))

(defn my-concat2 [lang coll]
  (reduce concat (map (fn [x] (my-concat1 lang x)) coll)))

(defn n-str-no-adj
  ([lang n]
   (if
     (= n 1)
     lang
     (n-str-no-adj lang n lang)))
  ([lang n res]
   (reduce (fn [x _] (my-concat2 lang x)) res (range (- n 1)))))
