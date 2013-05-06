; Day 1
(defn big [st n]
  (> (count st) n))

(big "7in7" 8)

(defn collection-type [col]
  (cond
    (vector? col) :vector
    (map? col) :map
    (list? col) :list))

(collection-type [1 2 3])
