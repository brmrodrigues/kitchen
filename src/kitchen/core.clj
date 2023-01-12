(ns kitchen.core
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def ^:const ORDER-RATE-PER-SEC 2)

; the order "temp" should match the shelves key in order to get its capacity
(def kitchen-shelves (atom {:hot      {:capacity 10
                                       :orders  []}
                            :cold     {:capacity 10
                                       :orders  []}
                            :frozen   {:capacity 10
                                       :orders  []}
                            :overflow {:capacity 15
                                       :orders  []}}))

(defn print-event
  [message]
  (prn message)
  (prn :kitchen-shelves @kitchen-shelves))

(defn available-shelf
  [shelves {:keys [temp] :as order}]
  (let [{:keys [orders capacity]} (get shelves temp)]
    (when (< (count orders) capacity) order)))

(defn remove-order-from-shelf
  [orders order]
  (remove #(= (:id %) (:id order)) orders))

(defn replace-order
  [shelves overflow-order order]
  (let [shelf (:temp overflow-order)]
    (-> shelves
        (update-in [:overflow :orders] remove-order-from-shelf overflow-order)
        (update-in [shelf :orders] conj overflow-order)
        (update-in [:overflow :orders] conj order))))

(defn discard-order-and-place-on-overflow
  [shelves order]
  (-> shelves
      (update-in [:overflow :orders] pop)
      (update-in [:overflow :orders] conj order)))

(defn deliver-order!
  [{:keys [db]} order]
  (let [shelf (:temp order)]
    (swap! db #(update-in % [shelf :orders] remove-order-from-shelf order))
    (print-event (str "Delivered order " (:id order)))))

(defn place-order-on-shelf!
  [db shelf-name order]
  (swap! db #(update-in %1 [shelf-name :orders] conj %2) order)
  (print-event (str "Placed order " (:id order) " on shelf " shelf-name)))

(defn place-order-on-overflow!
  [db order]
  (swap! db (fn [shelves order] (update-in shelves [:overflow :orders] conj order)) order)
  (print-event (str "Placed order " (:id order) " on overflow shelf")))


(defn replace-order-on-overflow!
 [db shelves overflow-shelf order]
 (if-let [overflow-order (some (partial available-shelf shelves) (:orders overflow-shelf))]
   (do (swap! db replace-order overflow-order order)
       (print-event (str "Moved order " (:id overflow-order) "from overflow shelf"
                         " into " (:temp overflow-order) " shelf"
                         " and placed order " (:id order)
                         " on overflow shelf ")))
   (let [last-order (-> shelves :overflow :orders peek)]
     (swap! db discard-order-and-place-on-overflow order)
     (print-event (str "No available shelf to place from overflow!"
                       "Discarded order " (:id last-order)
                       " and placed order " (:id order)
                       " on overflow shelf ")))))

(defn place-order!
  [{:keys [db]} order]
  (let [shelves @db
        shelf-name (:temp order)
        {:keys [capacity orders]} (get shelves shelf-name)
        {overflow-capacity :capacity overflow-orders :orders :as overflow-shelf} (:overflow shelves)]
    (cond
      (< (count orders) capacity) (place-order-on-shelf! db shelf-name order)
      (< (count overflow-orders) overflow-capacity) (place-order-on-overflow! db order)
      :else (replace-order-on-overflow! db shelves overflow-shelf order))))

(defn dispatch-order!
  [order]
  (let [delay-millis (* 1000 (+ 2 (rand-int 5)))
        system {:db kitchen-shelves}]
    (print-event (str "Order received: " (:id order)))
    (place-order! system order)
    (Thread/sleep delay-millis)
    (print-event "Courier arrived ")
    (deliver-order! system order)))

(defn start-ingestion
  [orders rate-per-sec]
  (let [delay-millis (/ 1000 rate-per-sec)]
    (doseq [order orders]
      (Thread/sleep delay-millis)
      (dispatch-order! order))))

(defn -main [& args]
  (let [json-string (slurp (io/resource "orders.json"))
        orders (json/read-str json-string
                              :key-fn keyword
                              :value-fn (fn [k v] (if (= :temp k) (keyword v) v)))]
    (start-ingestion orders ORDER-RATE-PER-SEC)))

