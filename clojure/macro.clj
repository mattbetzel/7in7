(def rev-inter
  (fn [coll n]
    (map #(map second %) (vals (group-by #(mod (first %) n) (map list (iterate inc 0) coll))))))

(rev-inter [1 2 3 4 5 6] 2)
(rev-inter (range 9) 3)

(fn [f coll]
  (when (seq coll)
    (if-let [x (f (first coll))] x (recur f (rest coll)))))
