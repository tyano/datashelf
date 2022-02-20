(ns datashelf.object-store
  (:refer-clojure :exclude [count get])
  (:require [clojure.core.async :refer [promise-chan]]
            [datashelf.cursor :refer [make-cursor-instance]]
            [datashelf.key-range :refer [key-range?] :as key-range]
            [datashelf.lang.core :refer [flatten-map keep-keys to-camel-case]]
            [datashelf.lang.string-list :refer [to-vector]]
            [datashelf.request :refer [setup-request-handlers]]
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
  ([{:keys [object-store]} value key options]
   {:pre [object-store]}
   (let [ch (promise-chan)
         data    (apply clj->js value (flatten-map options))
         request (if key
                   (.add object-store data key)
                   (.add object-store data))]
     (setup-request-handlers request ch)
     ch))
  
  ([object-store-instance value options]
   (add object-store-instance value nil options)))

(defn clear
  [{:keys [object-store]}]
  {:pre [object-store]}
  (let [ch (promise-chan)
        request (.clear object-store)]
    (setup-request-handlers request ch)
    ch))

(defn- resolve-key-range
  [maybe-key-range]
  (if (key-range? maybe-key-range) (key-range/to-js maybe-key-range) maybe-key-range))

(defn count
  ([{:keys [object-store]} query]
   {:pre [object-store]}
   (let [ch (promise-chan)
         request (if-let [range (resolve-key-range query)]
                   (.count object-store range)
                   (.count object-store))]
     (setup-request-handlers request ch)
     ch))
  
  ([instance]
   (count instance nil)))

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

(defn get
  [{:keys [object-store]} key]
  {:pre [object-store key]}
  (let [ch (promise-chan)
        request (.get object-store key)]
    (setup-request-handlers request ch #(js->clj %))
    ch))

(defn get-key
  [{:keys [object-store]} key options]
  {:pre [object-store key]}
  (let [ch (promise-chan)
        request (.getKey object-store (resolve-key-range key))]
    (setup-request-handlers request ch #(apply js->clj % (flatten-map options)))
    ch))

(defn get-all
  ([{:keys [object-store]} query count options]
   {:pre [object-store  
          (if count (or (zero? count) (pos-int? count)) true)]}
   (let [ch (promise-chan)
         range   (resolve-key-range query)
         request (cond
                   (and range count)
                   (.getAll object-store range count)
                   
                   range
                   (.getAll object-store range)
                   
                   :else
                   (.getAll object-store))]
     (setup-request-handlers request ch #(apply js->clj % (flatten-map options)))
     ch))
  
  ([instance query options]
   (get-all instance query nil options))
  
  ([instance options]
   (get-all instance nil options)))

(defn get-all-keys
  ([{:keys [object-store]} query count options]
   {:pre [object-store 
          (if count (or (zero? count) (pos-int? count)) true)]}
   (let [ch (promise-chan)
         range   (resolve-key-range query)
         request (cond
                   (and range count)
                   (.getAllKeys object-store range count)
                   
                   range
                   (.getAllKeys object-store range)
                   
                   :else
                   (.getAllKeys object-store))]
     (setup-request-handlers request ch #(apply js->clj % (flatten-map options)))
     ch))
  
  ([instance query options]
   (get-all-keys instance query nil options))
  
  ([instance options]
   (get-all-keys instance nil options)))

(defn index
  [{:keys [object-store]} index-name]
  {:pre [object-store index-name]}
  (make-index-instance (.index object-store index-name)))

(defn open-cursor
  [{:keys [object-store]} query direction]
  {:pre [object-store (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
  (let [ch (promise-chan)
        range (resolve-key-range query)
        request (cond
                  (and range direction)
                  (.openCursor object-store range (name direction))
                  
                  range
                  (.openCursor object-store range)
                  
                  :else
                  (.openCursor object-store))]
    (setup-request-handlers request ch make-cursor-instance)
    ch))

(defn open-key-cursor
  [{:keys [object-store]} query direction]
  {:pre [object-store (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
  (let [ch (promise-chan)
        range (resolve-key-range query)
        request (cond
                  (and range direction)
                  (.openKeyCursor object-store range (name direction))

                  range
                  (.openKeyCursor object-store range)

                  :else
                  (.openKeyCursor object-store))]
    (setup-request-handlers request ch make-cursor-instance)
    ch))

(defn put
  ([{:keys [object-store]} item key options]
   {:pre [object-store item]}
   (let [ch (promise-chan)
         clj->js-options (select-keys options [:keyword-fn])
         js->clj-options (select-keys options [:keywordize-keys])
         data    (apply clj->js item (flatten-map clj->js-options))
         request (if (some? key)
                   (.put object-store data key)
                   (.put object-store data))]
     (setup-request-handlers request ch #(apply js->clj % (flatten-map js->clj-options)))
     ch))
  
  ([instance item options]
   (put instance item nil options)))