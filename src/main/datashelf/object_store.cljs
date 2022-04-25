(ns datashelf.object-store
  (:refer-clojure :exclude [count get name key])
  (:require [clojure.core :as core]
            [clojure.core.async :refer [chan close! promise-chan put!]]
            [databox.core :as databox]
            [datashelf.cursor :refer [make-cursor-instance] :as csr]
            [datashelf.key-range :refer [resolve-key-range] :as key-range]
            [datashelf.lang.core :as lang]
            [datashelf.lang.string-list :refer [to-vector]]
            [datashelf.index :refer [make-index-instance]]
            [datashelf.request :refer [setup-request-handlers]]
            [datashelf.transaction :refer [make-transaction-instance]]
            [taoensso.timbre :refer-macros [debug] :as timbre]))

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
  ([{:keys [object-store]} value key {:keys [convert-value convert-result] :or {convert-value true convert-result true}}]
   {:pre [object-store value value]}
   (let [ch (promise-chan)
         data    (if convert-value
                   (if (boolean? convert-value)
                     (if (true? convert-value) (lang/clj->js value) value)
                     (lang/clj->js value convert-value))
                   value)
         
         request (if key
                   (.add object-store data key)
                   (.add object-store data))]
     (setup-request-handlers request ch (when convert-result
                                          (if (boolean? convert-result)
                                            (when (true? convert-result) lang/js->clj)
                                            #(lang/js->clj % convert-result))))
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
  ([{:keys [object-store]} item key {:keys [convert-value convert-result] :or {convert-value true convert-result true}}]
   {:pre [object-store item]}
   (let [ch (promise-chan)
         data    (if convert-value
                   (if (boolean? convert-value)
                     (if (true? convert-value) (lang/clj->js item) item)
                     (lang/clj->js item convert-value))
                   item)
         request (if (some? key)
                   (.put object-store data key)
                   (.put object-store data))]
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result)
                                                               (when (true? convert-result) lang/js->clj)
                                                               #(lang/js->clj % convert-result))))
     ch))

  ([instance item options]
   (put instance item nil options))

  ([instance item]
   (put instance item nil)))