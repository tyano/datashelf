(ns datashelf.query
  (:refer-clojure :exclude [count get name key])
  (:require [clojure.core :as core]
            [clojure.core.async :refer [>! chan close! go promise-chan put!]]
            [databox.core :as databox]
            [datashelf.cursor :refer [make-cursor-instance] :as csr]
            [datashelf.key-range :refer [key-range? resolve-key-range] :as key-range]
            [datashelf.request :refer [result-converter setup-request-handlers]]
            [taoensso.timbre :refer-macros [debug] :as timbre]))

(defprotocol Queriable
  (js-instance [self] "return a js-instance which this Queriable object hold. all query-method will be called onto the js-instance."))

(defn queriable?
  [obj]
  (satisfies? Queriable obj))

(defn count
  ([instance query]
   {:pre [(queriable? instance) (some? (js-instance instance))]}
   (let [js-obj (js-instance instance)
         ch (promise-chan)
         request (if-let [range (resolve-key-range query)]
                   (.count js-obj range)
                   (.count js-obj))]
     (setup-request-handlers request ch)
     ch))

  ([instance]
   (count instance nil)))

(defn get
  ([instance key {convert-result-opts :convert-result :or {convert-result-opts true}}]
   {:pre [(queriable? instance) (some? (js-instance instance)) key]}
   (let [js-obj (js-instance instance)
         ch (promise-chan)
         request (.get js-obj key)]
     (setup-request-handlers request ch (result-converter convert-result-opts))
     ch))

  ([instance key]
   (get instance key nil)))

(defn get-key
  ([instance key {convert-result-opts :convert-result :or {convert-result-opts true}}]
   {:pre [(queriable? instance) (some? (js-instance instance)) key]}
   (let [js-obj (js-instance instance)
         ch (promise-chan)
         request (.getKey js-obj (resolve-key-range key))]
     (setup-request-handlers request ch (result-converter convert-result-opts))
     ch))

  ([instance key]
   (get-key instance key nil)))


(defn get-all
  ([instance query count {convert-result-opts :convert-result :or {convert-result-opts true}}]
   {:pre [(queriable? instance) (some? (js-instance instance))
          (if count (or (zero? count) (pos-int? count)) true)]}
   (let [js-obj (js-instance instance)
         ch (promise-chan)
         range   (resolve-key-range query)
         request (cond
                   (and range count)
                   (.getAll js-obj range count)

                   range
                   (.getAll js-obj range)

                   :else
                   (.getAll js-obj))]
     (setup-request-handlers request ch (result-converter convert-result-opts))
     ch))

  ([instance query options]
   (get-all instance query nil options))

  ([instance options]
   (get-all instance nil options))

  ([instance]
   (get-all instance nil)))


(defn get-all-keys
  ([instance query count {convert-result-opts :convert-result :or {convert-result-opts true}}]
   {:pre [(queriable? instance) (some? (js-instance instance))
          (if count (or (zero? count) (pos-int? count)) true)]}
   (let [js-obj (js-instance instance)
         ch (promise-chan)
         range   (resolve-key-range query)
         request (cond
                   (and range count)
                   (.getAllKeys js-obj range count)

                   range
                   (.getAllKeys js-obj range)

                   :else
                   (.getAllKeys js-obj))]
     (setup-request-handlers request ch (result-converter convert-result-opts))
     ch))

  ([instance query options]
   (get-all-keys instance query nil options))

  ([instance options]
   (get-all-keys instance nil options))

  ([instance]
   (get-all-keys instance nil)))

(defn open-cursor
  ([instance range direction callback]
   {:pre [(queriable? instance) (some? (js-instance instance))
          (key-range? range)
          (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
   (let [js-obj (js-instance instance)
         result-ch (promise-chan)
         range     (resolve-key-range range)
         request   (cond
                     (and range direction)
                     (do
                       (debug "range-direction cursor")
                       (.openCursor js-obj range (core/name direction)))

                     range
                     (do
                       (debug "range cursor")
                       (.openCursor js-obj range))

                     :else
                     (do
                       (debug "default cursor")
                       (.openCursor js-obj)))]

     (set! (.-onsuccess request)
           (fn [_]
             (try
               (let [cursor (if-let [js-cursor (.-result request)]
                              (callback (make-cursor-instance js-cursor))
                              (callback nil))]
                 (if cursor
                   (csr/continue cursor)
                   (go
                     (>! result-ch (databox/success nil))
                     (close! result-ch))))
               (catch :default ex
                 (go
                   (>! result-ch (databox/failure ex))
                   (close! result-ch))))))

     (set! (.-onerror request)
           (fn [_]
             (let [error (.-error request)]
               (go
                 (>! result-ch (databox/failure error))
                 (close! result-ch)))))
     result-ch))

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
                            cursor))
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
  ([instance query direction callback]
   {:pre [(queriable? instance) (some? (js-instance instance))
          (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
   (let [js-obj (js-instance instance)
         range (resolve-key-range query)
         request (cond
                   (and range direction)
                   (.openKeyCursor js-obj range (core/name direction))

                   range
                   (.openKeyCursor js-obj range)

                   :else
                   (.openKeyCursor js-obj))]

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