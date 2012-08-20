(defn blank? [str]
  (every? #(Character/isWhitespace %) str))

(defn greeting
  "Returns a greeting of the form 'Hello, username.'"
  ([] (greeting "world"))
  ([username] (str "Hello, " username)))

(defn date
  [person-1 person-2 & chaperones]
  (println person-1 "and" person-2
           "went out with" (count chaperones) "chaperones."))

(defn indexable-word? [word]
  (> (count word) 2))

(require '[clojure.string :as str])
(filter indexable-word? (str/split "A fine day it is" #"\W+"))

(defn indexable-words [text]
  (let [indexable-word? (fn [w] (> (count w) 2))]
    (filter indexable-word? (str/split text #"\W+"))))

(defn make-greeter [greeting]
  (fn [person] (str greeting person)))

(defn greet-author [{fname :first-name}]
  (println "Hello," fname))

(defn greet-second [[_ x]]
  (println "Hello," x))

(defn ellipsize [phrase]
  (let [[x y z] (str/split phrase #"\W+")]
    (str/join " " [x y z "..."])))

(defn indexed [coll]
  (map-indexed vector coll))

(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))
