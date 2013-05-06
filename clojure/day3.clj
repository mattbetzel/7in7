;Day 3
(defn customers [lower upper]
  "An infinite seq of customer ids and pace"
  (let [ids (range)
        pace (repeatedly #(+ lower (rand-int (- upper lower))))]
    (map vector ids pace)))

(defn shop-valid? [{waiting :waiting}]
  "A shop can only have 3 customers in the waiting room"
  (<= (count waiting) 3))

(defn queue []
  "Create an empty queue"
  (clojure.lang.PersistentQueue/EMPTY))

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

(defn receptionist [_ shop-ref old-shop new-shop]
  "If the chair is emptied or a customer comes into an empty shop,
  take a customer to the chair"
  (let [chair-emptied (removed? [:chair] old-shop new-shop)
        first-customer (and (empty? (old-shop :waiting)) (not-empty (new-shop :waiting)))]
    (when (or chair-emptied first-customer) (println "Taking") (swap! shop-ref take-waiting-customer))))

(defn barber [cut-delay _ shop-ref old-shop new-shop]
  "If the chair is filled, finish the hair cut after delay"
  (let [chair-filled (added? [:chair] old-shop new-shop)]
    (when chair-filled (future (delayed cut-delay (println "Finished" (new-shop :chair)) (swap! shop-ref finish-hair-cut))))))

(defn run-customers [shop-ref customers]
  "Add the customers to the shop at a delayed rate"
  (doseq [[id enter-delay] customers]
    (delayed enter-delay (println "Adding" id) (swap! shop-ref add-customer id))))

(defn empty-shop [cut-delay]
  "Create an empty shop with a receptionist and barber"
  (-> (atom {:waiting (queue) :chair nil} :validator shop-valid?)
      (add-watch :receptionist receptionist)
      (add-watch :barber (partial barber cut-delay))))

(def my-shop (empty-shop 2000))

(run-customers my-shop (take 5 (customers 1000 3000)))

(count (@my-shop :waiting))
