(ns datashelf.index
  (:refer-clojure :exclude [get count name key])
  (:require [clojure.core :as core]
            [datashelf.object-store.instance :refer [make-object-store-instance]]
            [clojure.core.async :refer [chan close! promise-chan put!]]
            [datashelf.key-range :refer [resolve-key-range] :as key-range]
            [datashelf.request :refer [setup-request-handlers]]
            [datashelf.lang.core :as lang]
            [datashelf.cursor :refer [make-cursor-instance] :as csr])
  (:require-macros [taoensso.timbre :refer [debug]]))

(defn make-index-instance
  [js-index]
  {:index js-index})

(defn auto-locale?
  [{:keys [index]}]
  {:pre [index]}
  (.-isAutoLocale index))

(defn locale
  [{:keys [index]}]
  {:pre [index]}
  (.-locale index))

(defn name
  [{:keys [index]}]
  {:pre [index]}
  (.-name index))

(defn object-store
  [{:keys [index]}]
  {:pre [index]}
  (make-object-store-instance (.-objectStore index)))

(defn key-path
  [{:keys [index]}]
  {:pre [index]}
  (.-keyPath index))

(defn multi-entry?
  [{:keys [index]}]
  {:pre [index]}
  (.-multiEntry index))

(defn unique?
  [{:keys [index]}]
  {:pre [index]}
  (.-unique index))

(defn count
  ([{:keys [index]} query]
   {:pre [index]}
   (let [ch (promise-chan)
         request (if-let [range (resolve-key-range query)]
                   (.count index range)
                   (.count index))]
     (setup-request-handlers request ch)
     ch))

  ([instance]
   (count instance nil)))

(defn get
  ([{:keys [index]} key {:keys [convert-result] :or {convert-result true}}]
   {:pre [index key]}
   (let [ch (promise-chan)
         request (.get index key)]
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result)
                                                               (when (true? convert-result) lang/js->clj)
                                                               #(lang/js->clj % convert-result))))
     ch))

  ([instance key]
   (get instance key nil))
  
  ([instance]
   (get instance nil)))

(defn get-key
  ([{:keys [index]} key {:keys [convert-result] :or {convert-result true}}]
   {:pre [index key]}
   (let [ch (promise-chan)
         request (.getKey index (resolve-key-range key))]
     (setup-request-handlers request ch (when convert-result (if (boolean? convert-result)
                                                               (when (true? convert-result) lang/js->clj)
                                                               #(lang/js->clj % convert-result))))
     ch))

  ([instance key]
   (get-key instance key nil))
  
  ([instance]
   (get-key instance nil nil)))

(defn get-all
  ([{:keys [index]} query count {:keys [convert-result] :or {convert-result true}}]
   {:pre [index
          (if count (or (zero? count) (pos-int? count)) true)]}
   (let [ch (promise-chan)
         range   (resolve-key-range query)
         request (cond
                   (and range count)
                   (.getAll index range count)

                   range
                   (.getAll index range)

                   :else
                   (.getAll index))]
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
  ([{:keys [index]} query count {:keys [convert-result] :or {convert-result true}}]
   {:pre [index
          (if count (or (zero? count) (pos-int? count)) true)]}
   (let [ch (promise-chan)
         range   (resolve-key-range query)
         request (cond
                   (and range count)
                   (.getAllKeys index range count)

                   range
                   (.getAllKeys index range)

                   :else
                   (.getAllKeys index))]
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


(defn open-cursor
  ([{:keys [index] :as instance} query direction callback]
   {:pre [index (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
   (let [range   (resolve-key-range query)
         request (cond
                   (and range direction)
                   (do
                     (debug "range-direction cursor")
                     (.openCursor index range (core/name direction)))

                   range
                   (do
                     (debug "range cursor")
                     (.openCursor index range))

                   :else
                   (do
                     (debug "default cursor")
                     (.openCursor index)))]

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


(defn open-key-cursor
  ([{:keys [index] :as instance} query direction callback]
   {:pre [index (if direction (#{:next :nextunique :prev :prevunique} direction) true)]}
   (let [range   (resolve-key-range query)
         request (cond
                   (and range direction)
                   (.openKeyCursor index range (core/name direction))

                   range
                   (.openKeyCursor index range)

                   :else
                   (.openKeyCursor index))]

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