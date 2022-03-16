(println "pls type smth")
(def alphabet (str (read)))

(def alphabet-len (count alphabet))
; (println "pls input n")
; (def n (int (read)))

(defn is-last-equal [x y]
  (= (str (last x)) (str y)))

(defn smart-conj [s x y]
  (if (is-last-equal x y) s (conj s (str x y))))

(defn build-seq
  ([msg] (build-seq msg '[] 0))
  ([msg cur-seq i]
    (if (= i alphabet-len)
      cur-seq
      (recur msg (smart-conj cur-seq msg (nth alphabet i)) (inc i)))))

(defn concat-seqs
  ([msg] (concat-seqs (str msg) '[] 0))
  ([msg cur-seq i]
    (if (= i alphabet-len)
      cur-seq
      (recur msg (concat cur-seq (build-seq msg)) (inc i)))))

; (defn iter
;   ([] (iter '[] 0))
;   ([cur-seq i]
;     (if (= i alphabet-len)
;       cur-seq
;       (recur (concat cur-seq (concat-seqs (nth alphabet i))) (inc i)))))

(println (concat-seqs alphabet))
