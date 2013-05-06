;Day 3
(defn customers [lower upper]
  "An infinite seq of customer ids and pace"
  (let [ids (range)
        pace (repeatedly #(+ lower (rand-int (- upper lower))))]
    (map vector ids pace)))

(defn shop-valid? [{waiting :waiting}]
  "A shop can only have 3 customers in the waiting room"
  (<= (count waiting) 3))

(defn add-customer [shop customer]
  "Add a customer to the waiting room if there is an empty seat"
  (if (< (count (shop :waiting)) 3)
    (update-in shop [:waiting] conj customer)
    shop))

(defn take-waiting-customer [{:keys [waiting chair] :as shop}]
  "Move customer from waiting room to barber chair,
  if waiting customer is available and chair is empty"
  (if (and (not-empty waiting) (nil? chair))
    {:waiting (pop waiting) :chair (peek waiting)}
    shop))

(defn finish-hair-cut [{chair :chair :as shop}]
  "Remove current customer from barber chair"
  (assoc-in shop [:chair] nil))

(defmacro delayed [ms & body]
  "Wait the specified number of milliseconds and then execute body"
  `(do
     (Thread/sleep ~ms)
     ~@body))

(defn added? [ks old-state new-state]
  "Returns truthy if a value wasn't present in old map but is present in new map"
  (and (not (get-in old-state ks)) (get-in new-state ks)))

(defn removed? [ks old-state new-state]
  "Returns truthy if a value was present in old map but isn't present in new map"
  (and (get-in old-state ks) (not (get-in new-state ks))))

(defn receptionist []
  "If the chair is emptied or a customer comes into an empty shop,
  take a customer to the chair"
  (fn [_ shop-ref old-shop new-shop]
    (let [chair-emptied (removed? [:chair] old-shop new-shop)
          first-customer (and (empty? (old-shop :waiting)) (not-empty (new-shop :waiting)))]
      (when (or chair-emptied first-customer) (println "Taking") (swap! shop-ref take-waiting-customer)))))

(defn barber [cut-delay]
  "If the chair is filled, finish the hair cut after delay"
  (fn [_ shop-ref old-shop new-shop]
    (let [chair-filled (added? [:chair] old-shop new-shop)]
      (when chair-filled (future (delayed cut-delay (println "Finished" (new-shop :chair)) (swap! shop-ref finish-hair-cut)))))))

(defn run-customers [shop-ref customers]
  "Add the customers to the shop at a delayed rate"
  (doseq [[id enter-delay] customers]
    (delayed enter-delay (println "Adding" id) (swap! shop-ref add-customer id))))

(defn queue []
  "Create an empty queue"
  (clojure.lang.PersistentQueue/EMPTY))

(defn empty-shop [& watcher-opts]
  "Create an empty shop with the provided watchers"
  (let [watchers (partition 2 watcher-opts)
        shop-ref (atom {:waiting (queue) :chair nil} :validator shop-valid?)
        reduce-fn (fn [shop-ref [watch-key watcher]] (add-watch shop-ref watch-key watcher))]
    (reduce reduce-fn shop-ref watchers)))

(def my-shop (empty-shop :barber (barber 200) :receptionist (receptionist)))

(run-customers my-shop (take 50 (customers 100 300)))

(count (@my-shop :waiting))
