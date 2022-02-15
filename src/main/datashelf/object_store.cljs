(ns datashelf.object-store
  (:require [datashelf.lang.core :refer [keep-keys to-camel-case]]
            [datashelf.lang.string-list :refer [to-vector]]
            [datashelf.database :refer [make-transaction-instance]]
            [clojure.core.async :refer [promise-chan go >! close!]]
            [databox.core :as databox]))

(defn make-index-instance
  [js-index]
  {:index js-index})

(defn create-index
  [{:keys [object-store]} key-path & [options]]
  {:pre [object-store key-path]}
  (let [index-data (if options
                     (let [js-opts (-> options
                                       (keep-keys #(-> % name to-camel-case))
                                       (clj->js))]
                       (.createIndex object-store key-path js-opts))
                     (.createIndex object-store key-path))]
    (make-index-instance index-data)))

(defn index-names
  [{:keys [object-store]}]
  {:pre [object-store]}
  (to-vector (.-index-names object-store)))

(defn key-path
  [{:keys [object-store]}]
  {:pre [object-store]}
  (.-keyPath object-store))

(defn name
  [{:keys [object-store]}]
  {:pre [object-store]}
  (.-name object-store))

(defn transaction
  [{:keys [object-store]}]
  {:pre [object-store]}
  (make-transaction-instance (.-transaction object-store)))

(defn auto-increment
  [{:keys [object-store]}]
  {:pre [object-store]}
  (.-autoIncrement object-store))

(defn add
  [{:keys [object-store]} value key]
  {:pre [object-store]}
  (let [ch (promise-chan)
        request (if key
                  (.add object-store value key)
                  (.add object-store value))]
    (set! (.-onsuccess request)
          (fn [_]
            (let [result (.-result request)]
              (go
                (>! ch (databox/success result))
                (close! ch)))))
    
    (set! (.-onerror request)
          (fn [_]
            (let [error (.-error request)]
              (go
                (>! ch (databox/failure error))
                (close! ch)))))
    
    ch))