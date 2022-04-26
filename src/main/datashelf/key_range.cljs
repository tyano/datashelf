(ns datashelf.key-range
  (:require [datashelf.request :as req]))

(defrecord KeyRange [key-range])

(defn make-key-range-instance
  [key-range]
  (when key-range
    (->KeyRange key-range)))

(defn key-range?
  [maybe-key-range]
  (and (some? maybe-key-range) 
       (instance? KeyRange maybe-key-range)))

(defn to-js
  [{:keys [key-range]}]
  key-range)

(defn lower-bound
  ([lower open {convert-value-opts :convert-value :or {convert-value-opts true}}]
   {:pre [lower (boolean? open)]}
   (let [lower (req/convert-value lower convert-value-opts)]
     (make-key-range-instance (js/IDBKeyRange.lowerBound lower open))))
  ([lower open]
   (lower-bound lower open nil))
  ([lower]
   (lower-bound lower false)))

(defn upper-bound
  ([upper open {convert-value-opts :convert-value :or {convert-value-opts true}}]
   (let [upper (req/convert-value upper convert-value-opts)]
     (make-key-range-instance (js/IDBKeyRange.upperBound upper open))))
  ([upper open]
   (upper-bound upper open nil))
  ([upper]
   (upper-bound upper false)))

(defn only
  ([value {convert-value-opts :convert-value :or {convert-value-opts true}}]
   {:pre [value]}
   (let [value (req/convert-value value convert-value-opts)]
     (make-key-range-instance (js/IDBKeyRange.only value))))
  ([value]
   (only value nil)))

(defn bound
  ([lower upper lower-open upper-open {convert-value-opts :convert-value :or {convert-value-opts true}}]
   {:pre [lower upper (boolean? lower-open) (boolean? upper-open)]}
   (let [lower (req/convert-value lower convert-value-opts)
         upper (req/convert-value upper convert-value-opts)]
     (make-key-range-instance (js/IDBKeyRange.bound lower upper lower-open upper-open))))
  
  ([lower upper lower-open upper-open]
   (bound lower upper lower-open upper-open nil))
  
  ([lower upper lower-open]
   (bound lower upper lower-open false))
  
  ([lower upper]
   (bound lower upper false)))

(defn lower
  [{:keys [key-range]}]
  {:keys [key-range]}
  (.-lower key-range))

(defn upper
  [{:keys [key-range]}]
  {:keys [key-range]}
  (.-upper key-range))

(defn lower-open
  [{:keys [key-range]}]
  {:keys [key-range]}
  (.-lowerOpen key-range))

(defn upper-open
  [{:keys [key-range]}]
  {:keys [key-range]}
  (.-upperOpen key-range))

(defn resolve-key-range
  [maybe-key-range]
  (if (key-range? maybe-key-range) (to-js maybe-key-range) maybe-key-range))