(ns kitchen.core
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def ^:const ORDER-RATE-PER-SEC 1)
(def ^:const COURIER-RATE-RANGE-PER-SEC [6 10])

; the order "temp" should match the shelves key in order to get its capacity
(def kitchen-shelves (atom {:hot      {:capacity 3
                                       :orders  []}
                            :cold     {:capacity 3
                                       :orders  []}
                            :frozen   {:capacity 3
                                       :orders  []}
                            :overflow {:capacity 4
                                       :orders  []}}))

(defn- courier-delay-secs
  [{:keys [courier-arrival-rate]}]
  (let [[start end] courier-arrival-rate]
    (* 1000 (+ start (rand-int (- end 1))))))

(defn print-event
  "formats the log message for each kitchen event"
  [db message]
  (->> (java.util.Date.)
       (.format (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm:ss"))
       (format "[%s] ")
       print)
  (prn message)
  (prn (format "kitchen-shelves: %s" @db))
  (prn))

(defn available-shelf
  "it simply checks if the shelf is available according to its capacity"
  [shelves {:keys [temp] :as order}]
  (let [{:keys [orders capacity]} (get shelves temp)]
    (when (< (count orders) capacity) order)))

(defn remove-order-from-shelf
  [orders order]
  (remove #(= (:id %) (:id order)) orders))

(defn replace-order
  "remove the order from overflow, adds it to a shelf and place the new order on overflow"
  [shelves overflow-order order]
  (let [shelf (:temp overflow-order)]
    (-> shelves
        (update-in [:overflow :orders] remove-order-from-shelf overflow-order)
        (update-in [shelf :orders] conj overflow-order)
        (update-in [:overflow :orders] conj order))))

(defn discard-order-and-place-on-overflow
  [shelves order]
  (-> shelves
      (update-in [:overflow :orders] drop-last)
      (update-in [:overflow :orders] conj order)))

(defn place-order-on-shelf!
  [db shelf-name order]
  (swap! db #(update-in %1 [shelf-name :orders] conj %2) order)
  (print-event db (str "PLACED-[SHELF] order " (:name order) " ID: " (:id order) " on shelf " shelf-name)))

(defn place-order-on-overflow!
  [db order]
  (swap! db (fn [shelves order] (update-in shelves [:overflow :orders] conj order)) order)
  (print-event db (str "PLACED-[OVERFLOW] order " (:name order) " ID: " (:id order) " on overflow shelf")))

(defn replace-order-on-overflow!
 [db shelves overflow-shelf order]
 (if-let [overflow-order (some (partial available-shelf shelves) (:orders overflow-shelf))]
   (do (swap! db replace-order overflow-order order)
       (print-event db (str "MOVED order " (:name overflow-order) " " (:id overflow-order) "from overflow shelf"
                            " into " (:temp overflow-order) " shelf"
                            " and placed order " (:id order)
                            " on overflow shelf ")))
   (let [last-order (-> shelves :overflow :orders last)]
     (swap! db discard-order-and-place-on-overflow order)
     (print-event db (str "DISCARDED order " (:name last-order) " " (:id last-order)
                          " and placed order " (:name order) " " (:id order)
                          " on overflow shelf")))))

;; Kitchen steps events
(defn receive-order!
 [{:keys [db]} order]
 (print-event db (str "RECEIVED order " (:name order) " ID: " (:id order))))

(defn cook-order!
  [{:keys [db]} order]
  (print-event db (str "COOKED order " (:name order) " ID: " (:id order))))

(defn deliver-order!
  [{:keys [db]} order]
  (print-event db (str "DELIVERED order " (:name order) " ID:" (:id order))))

(defn pick-up-order!
  [{:keys [db]} order]
  (let [shelf (:temp order)]
    (swap! db #(update-in % [shelf :orders] remove-order-from-shelf order))
    (print-event db (str "PICKED-UP order " (:name order) " ID: " (:id order)))))

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
;;

(defn consume-orders!
  "execute the kitchen process step by step"
  [system orders]
  (let [delay-millis (* 1000 (:order-ingestion-rate system))]
    (doseq [order orders]
      (Thread/sleep delay-millis)
      (receive-order! system order)
      (cook-order! system order)
      (place-order! system order)
      (future (Thread/sleep (courier-delay-secs system))
              (pick-up-order! system order)
              (deliver-order! system order)))))

(defn -main [& args]
  (let [json-string (slurp (io/resource "orders.json"))
        orders (json/read-str json-string
                              :key-fn keyword
                              :value-fn (fn [k v] (if (= :temp k) (keyword v) v)))
        system {:db kitchen-shelves
                :order-ingestion-rate ORDER-RATE-PER-SEC
                :courier-arrival-rate COURIER-RATE-RANGE-PER-SEC}]
    (consume-orders! system orders)))
