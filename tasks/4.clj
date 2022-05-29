(ns task4.core)

(defn constant [num]
  {:pre [(or (= num 1) (= num 0))]}
  (list ::const num))

(defn constant? [c]
  (= (first c) ::const))

(defn constant-value [c]
  (second c))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [v]
  (= (first v) ::var))

(defn variable-name [v]
  (second v))

(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2))))

(defn && [expr & rest]
  (if (empty? rest)
    expr
    (cons ::conj (cons expr rest))))

(defn &&? [expr]
  (= ::conj (first expr)))

(defn || [expr & rest]
  (if (empty? rest)
    expr
    (cons ::disj (cons expr rest))))

(defn ||? [expr]
  (= ::disj (first expr)))

(defn -> [expr & rest]
  (if (empty? rest)
    expr
    (cons ::impl (cons expr rest))))

(defn ->? [expr]
  (= ::impl (first expr)))

(defn neg [expr]
  (if (= ::neg (first expr))
    (rest expr)
    (cons ::neg expr)))

(defn neg? [expr]
  (= ::neg (first expr)))

(defn args [expr]
  (rest expr))

(defn neg-variable? [expr]
  (and (= ::neg (first expr)) (variable? (args expr))))

(defn sub-val [expr v val]
  {:pre [(and (variable? v) (or (= val 0) (= val 1)))]}
  (let [sub-rules (list
                    [(fn [expr] (same-variables? expr v))
                     (fn [expr] (constant val))]
                    [(fn [expr] (not (or (constant? expr) (variable? expr))))
                     (fn [expr] (cons (first expr) (map (fn [x] (sub-val x v val)) (args expr))))]
                    [(fn [expr] true) (fn [expr] expr)])]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         sub-rules)
   expr)))

(defn impl-elim [expr]
  (let [elim-rules (list
                     [->? (fn [expr] (||
                                       (neg (impl-elim (first (args expr))))
                                       (impl-elim (second (args expr)))))]
                     [(fn [expr] (not (or (constant? expr) (variable? expr))))
                      (fn [expr] (cons (first expr) (map impl-elim (args expr))))]
                     [(fn [expr] true) (fn [expr] expr)])]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         elim-rules)
   expr)))

(defn de-morgans-laws [expr]
  (let [de-morgans-rules (list
                           [(fn [expr] (and (neg? expr) (not (variable? (args expr)))))
                            (fn [expr] (if (||? (args expr))
                                         (&& (neg (de-morgans-laws (first (args (args expr)))))
                                             (neg (de-morgans-laws (second (args (args expr))))))
                                         (|| (de-morgans-laws (neg (first (args (args expr)))))
                                             (de-morgans-laws (neg (second (args (args expr))))))))]
                           [(fn [expr] (not (or (constant? expr)
                                                (variable? expr)
                                                (variable? (args expr)))))
                            (fn [expr] (cons (first expr) (map de-morgans-laws (args expr))))]
                           [(fn [expr] true) (fn [expr] expr)])]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         de-morgans-rules)
   expr)))

(defn double-neg-elim [expr]
  (let [double-neg-elim-rules (list
                                [(fn [expr] (and (neg? expr) (not (variable? (args expr)))))
                                 (fn [expr] (if (neg? (args expr))
                                              (cons (first (args expr))
                                                    (map double-neg-elim
                                                         (args (args expr))))
                                              (cons (first expr) (map double-neg-elim
                                                                      (args expr)))))]
                                [(fn [expr] (not (or
                                                   (constant? expr)
                                                   (variable? expr)
                                                   (variable? (args expr)))))
                                 (fn [expr] (cons (first expr) (map double-neg-elim (args expr))))]
                                [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           double-neg-elim-rules)
     expr)))

(defn no-disj [expr]
  (cond
    (or (variable? expr) (neg-variable? expr)) true
    (||? expr) false
    true (every? no-disj (args expr))))

(defn distributive-pr [expr]
  (let [distr-rules (list
                      [&&? (fn [expr] (cond
                                        (||? (first (args expr)))
                                        (distributive-pr (||
                                                            (distributive-pr
                                                                 (&&
                                                                   (first (args (first (args expr))))
                                                                   (second (args expr))))
                                                            (distributive-pr
                                                                 (&&
                                                                   (second (args (first (args expr))))
                                                                   (second (args expr))))))
                                        (||? (second (args expr)))
                                        (distributive-pr (||
                                                            (distributive-pr
                                                                 (&&
                                                                   (first (args expr))
                                                                   (first (args (second (args expr))))))
                                                            (distributive-pr
                                                                 (&&
                                                                   (first (args expr))
                                                                   (second (args (second (args expr))))))))
                                        true (if (no-disj expr)
                                               expr
                                               (distributive-pr
                                                 (cons (first expr)
                                                       (map distributive-pr (args expr)))))))]
                      [(fn [expr] (not (or (constant? expr) (variable? expr) (neg-variable? expr))))
                       (fn [expr] (cons (first expr) (map distributive-pr (args expr))))]
                      [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           distr-rules)
     expr)))

(defn idempotence-pr [expr]
  (let [idemp-rules (list
                      [(fn [expr] (or (||? expr) (&&? expr)))
                       (fn [expr] (if (same-variables? (first (args expr)) (second (args expr)))
                                    (first (args expr))
                                    (cons (first expr) (map idempotence-pr (args expr)))))]
                      [(fn [expr] (not (or (constant? expr) (variable? expr) (neg-variable? expr))))
                       (fn [expr] (cons (first expr) (map idempotence-pr (args expr))))]
                      [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           idemp-rules)
     expr)))

(defn associativity-pr1 [args pred conn]
  (apply conn (reverse (reduce
                         (fn [acc arg] (if (pred arg) (concat acc (rest arg)) (conj acc arg)))
                         '()
                         args))))

(defn associativity-pr [expr]
  (let [assoc-rules (list
                              [||? (fn [expr] (apply || (args (associativity-pr1
                                                                (map associativity-pr (args expr))
                                                                ||?
                                                                ||))))]
                              [&&? (fn [expr] (apply && (args (associativity-pr1
                                                                 (map associativity-pr (args expr))
                                                                 &&?
                                                                 &&))))]
                              [(fn [expr] (not (or (constant? expr) (variable? expr) (neg-variable? expr))))
                               (fn [expr] (cons (first expr) (map associativity-pr (args expr))))]
                              [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           assoc-rules)
     expr)))

(defn transform-to-dnf [expr]
  (associativity-pr
    (idempotence-pr
      (distributive-pr
        (de-morgans-laws
          (impl-elim expr))))))
