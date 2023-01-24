(ns kitchen.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [kitchen.core :as core]))

(def initial-shelves
  {:hot      {:capacity 2
              :orders   []}
   :cold     {:capacity 2
              :orders   []}
   :frozen   {:capacity 2
              :orders   []}
   :overflow {:capacity 3
              :orders   []}})

(def test-db (atom initial-shelves))

(defn- get-shelves! []
  (deref test-db))

(def frozen-1-cold-2-fixtures
   [{:id (random-uuid)
     :name "Banana Split"
     :temp :frozen}
    {:id (random-uuid)
     :name "Acai Bowl"
     :temp :cold}
    {:id (random-uuid)
     :name "Yogurt"
     :temp :cold}])

(def overflow-fixtures
  [{:id (random-uuid)
    :name "Cobb Salad"
    :temp :cold}
   {:id (random-uuid)
    :name "Cottage Cheese"
    :temp :cold}
   {:id (random-uuid)
    :name "Coke"
    :temp :cold}])

(def overflow-discard-fixtures
  [{:id (random-uuid)
    :name "McFlury"
    :temp :frozen}
   {:id (random-uuid)
    :name "Chocolate Gelato"
    :temp :frozen}])

(def overflow-replacement-fixtures
  [{:id (random-uuid)
    :name "Kale Salad"
    :temp :cold}])

(defn- consume-orders!
  [orders order-ingestion-rate courier-arrival-rate]
  (core/consume-orders! {:db test-db
                         :order-ingestion-rate order-ingestion-rate
                         :courier-arrival-rate courier-arrival-rate}
                        orders))

(deftest no-overflow-orders-test
 (reset! test-db initial-shelves)

 (testing "courier arrival rate is faster than order's"
   (consume-orders! frozen-1-cold-2-fixtures 3 [0 1])
   (Thread/sleep 1000)
   (testing "all orders are delivered"
     (let [shelves (get-shelves!)]
       (is (empty? (-> shelves :frozen :orders)))
       (is (empty? (-> shelves :cold :orders)))))))

(deftest overflow-orders-test
  (reset! test-db initial-shelves)

  (testing "order's arrival is faster than courirer's => overflow shelf"
    (consume-orders! overflow-fixtures 1 [3 4])
    (Thread/sleep 6000)
    (testing "there is at least one order on overflow shelf"
      (let [shelves (get-shelves!)]
        (is (< 0 (count (-> shelves :overflow :orders))))))))


(deftest overflow-discard-test
  (reset! test-db initial-shelves)

  (testing "order's arrival is faster than courirer's => overflow shelf => waste"
    (consume-orders! (concat frozen-1-cold-2-fixtures overflow-fixtures overflow-discard-fixtures)
                     1 [10 11])
    (Thread/sleep 10000)
    (testing "there are at least one order on overflow shelf"
      (let [shelves (get-shelves!)]
        (is (nil? (some #(= "Coke"
                            (:name %))
                        (-> shelves :overflow :orders))))))))
