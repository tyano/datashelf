(ns datashelf.object-store
  (:refer-clojure :exclude [count get name key])
  (:require [clojure.core.async :refer [promise-chan]]
            [datashelf.index :refer [make-index-instance]]
            [datashelf.key-range :refer [resolve-key-range] :as key-range]
            [datashelf.lang.core :as lang]
            [datashelf.lang.string-list :refer [to-vector]]
            [datashelf.request :refer [setup-request-handlers convert-value result-converter]]
            [datashelf.transaction :refer [make-transaction-instance]]))

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
  ([{:keys [object-store]} value key {convert-value-opts :convert-value convert-result-opts :convert-result :or {convert-value-opts true convert-result-opts true}}]
   {:pre [object-store value value]}
   (let [ch (promise-chan)
         data    (convert-value value convert-value-opts)
         request (if key
                   (.add object-store data key)
                   (.add object-store data))]
     (setup-request-handlers request ch (result-converter convert-result-opts))
     ch))
  
  ([object-store-instance value options]
   (add object-store-instance value nil options))
  
  ([object-store-instance value]
   (add object-store-instance value nil)))

(defn clear
  [{:keys [object-store]}]
  {:pre [object-store]}
  (let [ch (promise-chan)
        request (.clear object-store)]
    (setup-request-handlers request ch)
    ch))

(defn create-index
  [{:keys [object-store]} index-name key-path & [options]]
  {:pre [object-store index-name key-path]}
  (let [index-data (if options
                     (let [js-opts (lang/clj->js options {:camelcasify-keys true})]
                       (.createIndex object-store index-name key-path js-opts))
                     (.createIndex object-store index-name key-path))]
    (make-index-instance index-data)))

(defn index
  [{:keys [object-store]} index-name]
  {:pre [object-store index-name]}
  (make-index-instance (.index object-store index-name)))

(defn delete
  [{:keys [object-store]} key]
  {:pre [object-store key]}
  (let [ch (promise-chan)
        request (.delete object-store (resolve-key-range key))]
    (setup-request-handlers request ch)
    ch))

(defn delete-index
  [{:keys [object-store]} index-name]
  {:pre [object-store index-name]}
  (.deleteIndex object-store index-name))

(defn put
  ([{:keys [object-store]} item key {convert-value-opts :convert-value convert-result-opts :convert-result :or {convert-value-opts true convert-result-opts true}}]
   {:pre [object-store item]}
   (let [ch (promise-chan)
         data    (convert-value item convert-value-opts)
         request (if (some? key)
                   (.put object-store data key)
                   (.put object-store data))]
     (setup-request-handlers request ch (result-converter convert-result-opts))
     ch))

  ([instance item options]
   (put instance item nil options))

  ([instance item]
   (put instance item nil)))