(ns datashelf.cursor
  (:refer-clojure :exclude [update])
  (:require [clojure.core.async :refer [promise-chan]]
            [datashelf.lang.core :refer [flatten-map]]
            [datashelf.request :refer [setup-request-handlers]]))

(defn make-cursor-instance
  [js-cursor]
  {:cursor js-cursor})

(defn source
  [{:keys [cursor]}]
  (.-source cursor))

(defn direction
  [{:keys [cursor]}]
  (some-> (.-direction cursor)
          (keyword)))

(defn key
  [{:keys [cursor]}]
  (.-key cursor))

(defn primary-key
  [{:keys [cursor]}]
  (.-primaryKey cursor))

(defn request
  [{:keys [cursor]}]
  (.-request cursor))

(defn value
  [{:keys [cursor]}]
  (.-value cursor))

(defn advance
  [{:keys [cursor] :as instance} count]
  (.advance cursor count)
  instance)

(defn continue
  ([{:keys [cursor] :as instance} key]
   (if (some? key)
     (.continue cursor key)
     (.continue cursor))
   instance)
  
  ([instance]
   (continue instance nil)))

(defn continue-primary-key
  [{:keys [cursor] :as instance} key primary-key]
  (.continuePrimaryKey cursor key primary-key)
  instance)

(defn delete
  [{:keys [cursor] :as instance}]
  (.delete cursor)
  instance)

(defn update
  [{:keys [cursor]} value options]
  (let [ch (promise-chan)
        data    (apply clj->js value (flatten-map options))
        request (.update cursor data)]
    (setup-request-handlers request ch)
    ch))