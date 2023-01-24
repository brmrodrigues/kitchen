(ns kitchen.place-order-test
  (:require [clojure.test :refer [deftest testing is]]
            [kitchen.core :as core]
            [matcher-combinators.matchers :refer [embeds]]
            [matcher-combinators.test :refer [match?]]))

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

(defn- place-orders!
  [orders]
  (doseq [order orders]
    (core/place-order! {:db test-db} order)))

(defn- pick-up-order!
  [order]
  (core/pick-up-order! {:db test-db} order))

(deftest place-order-test
  (reset! test-db initial-shelves)
  (place-orders! frozen-1-cold-2-fixtures)
  (let [shelves (get-shelves!)]
    (testing "fill frozen shelf with only one order"
      (is (match? (embeds [{:name "Banana Split"}])
                  (-> shelves :frozen :orders))))
    (testing "fill cold shelf to its max capacity"
      (let [{:keys [capacity orders]} (-> shelves :cold)]
        (is (match? (embeds [{:name "Acai Bowl"}
                             {:name "Yogurt"}])
                    orders))
        (is (= (count orders) capacity)))))

  (place-orders! overflow-fixtures)
  (let [shelves (get-shelves!)]
    (testing "fill overflow shelf with cold orders only"
      (let [{:keys [capacity orders]} (-> shelves :overflow)]
        (is (match? (embeds [{:name "Cobb Salad" :temp :cold}
                             {:name "Cottage Cheese" :temp :cold}
                             {:name "Coke" :temp :cold}])
                    orders))
        (is (= (count orders) capacity)))))

  (place-orders! overflow-discard-fixtures)
  (let [shelves (get-shelves!)]
    (testing "fill frozen shelf to its max capacity"
      (let [{:keys [capacity orders]} (-> shelves :frozen)]
        (is (match? (embeds [{:name "Banana Split"}
                             {:name "McFlury"}])
                    orders))
        (is (= (count orders) capacity))))
    (testing "Discarded Coke order to place Chocolate Gelato into the overflow shelf"
      (let [{:keys [capacity orders]} (-> shelves :overflow)]
        (is (match? (embeds [{:name "Cobb Salad"}
                             {:name "Cottage Cheese"}
                             {:name "Chocolate Gelato"}])
                    orders))
        (is (= (count orders) capacity)))))

  (pick-up-order! (first frozen-1-cold-2-fixtures))
  (let [shelves (get-shelves!)]
    (testing "Deliver Banana Split order"
      (let [{:keys [capacity orders]} (-> shelves :frozen)]
        (is (match? (embeds [{:name "McFlury"}])
                    orders))
        (is (< (count orders) capacity)))))

  (place-orders! overflow-replacement-fixtures)
  (let [shelves (get-shelves!)]
    (testing "Moved Chocolate Gelato from overflow to frozen shelf and placed Kabe Salad order on overflow shelf"
      (let [{overflow-orders :orders} (-> shelves :overflow)
            {frozen-orders :orders} (-> shelves :frozen)]
        (is (match? (embeds [{:name "Cobb Salad"}
                             {:name "Cottage Cheese"}])
                    overflow-orders))
        (is (match? (embeds [{:name "McFlury"}
                             {:name "Chocolate Gelato"}])
                    frozen-orders))))))
