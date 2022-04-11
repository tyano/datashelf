(ns datashelf.object-store
  (:refer-clojure :exclude [count get name key])
  (:require [clojure.core :as core]
            [clojure.core.async :refer [chan close! promise-chan put!]]
            [databox.core :as databox]
            [datashelf.cursor :refer [make-cursor-instance] :as csr]
            [datashelf.key-range :refer [key-range?] :as key-range]
            [datashelf.lang.core :as lang]
            [datashelf.lang.string-list :refer [to-vector]]
            [datashelf.object-store.instance :refer [make-index-instance]]
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
  [{:keys [object-store]} index-name key-path & [options]]
  {:pre [object-store index-name key-path]}
  (let [index-data (if options
                     (let [js-opts (lang/clj->js options {:camelcasify-keys true})]
                       (.createIndex object-store index-name key-path js-opts))
                     (.createIndex object-store index-name key-path))]
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
  ([{:keys [object-store]} key {:keys [convert-result] :or {convert-result true}}]
   {:pre [object-store key]}
   (let [ch (promise-chan)
         request (.get object-store key)]
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result) 
                                                               (when (true? convert-result) lang/js->clj) 
                                                               #(lang/js->clj % convert-result))))
     ch))
  
  ([instance key]
   (get instance key nil)))

(defn get-key
  ([{:keys [object-store]} key {:keys [convert-result] :or {convert-result true}}]
   {:pre [object-store key]}
   (let [ch (promise-chan)
         request (.getKey object-store (resolve-key-range key))]
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result)
                                                              (when (true? convert-result) lang/js->clj) 
                                                               #(lang/js->clj % convert-result))))
     ch))
  
  ([instance key]
   (get-key instance key nil)))

(defn get-all
  ([{:keys [object-store]} query count {:keys [convert-result] :or {convert-result true}}]
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
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result) 
                                                               (when (true? convert-result) lang/js->clj) 
                                                               #(lang/js->clj % convert-result))))
     ch))
  
  ([instance query options]
   (get-all instance query nil options))
  
  ([instance options]
   (get-all instance nil options))
  
  ([instance]
   (get-all instance nil)))

(defn get-all-keys
  ([{:keys [object-store]} query count {:keys [convert-result] :or {convert-result true}}]
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
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result)
                                                               (when (true? convert-result) lang/js->clj)
                                                               #(lang/js->clj % convert-result))))
     ch))
  
  ([instance query options]
   (get-all-keys instance query nil options))
  
  ([instance options]
   (get-all-keys instance nil options))
  
  ([instance]
   (get-all-keys instance nil)))

(defn index
  [{:keys [object-store]} index-name]
  {:pre [object-store index-name]}
  (make-index-instance (.index object-store index-name)))

(defn open-cursor  
  ([{:keys [object-store] :as instance} query direction callback]
   {:pre [object-store (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
   (let [range (resolve-key-range query)
         request (cond
                   (and range direction)
                   (do
                     (debug "range-direction cursor")
                     (.openCursor object-store range (core/name direction)))

                   range
                   (do
                     (debug "range cursor")
                     (.openCursor object-store range))

                   :else
                   (do
                     (debug "default cursor")
                     (.openCursor object-store)))]

     (set! (.-onsuccess request)
           (fn [_]
             (if-let [js-cursor (.-result request)]
               (callback (make-cursor-instance js-cursor))
               (callback nil))))

     (set! (.-onerror request)
           (fn [_]
             (let [error (.-error request)]
               (throw error))))
     instance))
  
  ([instance query callback]
   (open-cursor instance query nil callback))
  
  ([instance callback]
   (open-cursor instance nil callback)))

(defn value-chan
  ([instance query direction options]
   (let [ch (chan)]
     (open-cursor instance
                  query
                  direction
                  (fn [cursor]
                    (if cursor
                      (try
                        (let [v (csr/value cursor options)]
                          (when (put! ch (databox/success v))
                            (csr/continue cursor)))
                        (catch :default ex
                          (put! ch (databox/failure ex))
                          (close! ch)))
                      (close! ch))))
     ch))
  
  ([instance query options]
   (value-chan instance query nil options))
  
  ([instance options]
   (value-chan instance nil options))
  
  ([instance]
   (value-chan instance nil)))

(defn open-key-cursor
  ([{:keys [object-store] :as instance} query direction callback]
   {:pre [object-store (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
   (let [range (resolve-key-range query)
         request (cond
                   (and range direction)
                   (.openKeyCursor object-store range (core/name direction))

                   range
                   (.openKeyCursor object-store range)

                   :else
                   (.openKeyCursor object-store))]

     (set! (.-onsuccess request)
           (fn [_]
             (when-let [js-cursor (.-result request)]
               (callback (make-cursor-instance js-cursor)))))

     (set! (.-onerror request)
           (fn [_]
             (let [error (.-error request)]
               (throw error))))
     instance))
  
  ([instance query callback]
   (open-key-cursor instance query nil callback))
  
  ([instance callback]
   (open-key-cursor instance nil callback)))

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