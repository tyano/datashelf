(ns datashelf.key-range)

(defrecord KeyRange [key-range])

(defn make-key-range-instance
  [key-range]
  (->KeyRange key-range))

(defn key-range?
  [maybe-key-range]
  (and (some? maybe-key-range) 
       (instance? KeyRange maybe-key-range)))

(defn to-js
  [{:keys [key-range]}]
  key-range)

(defn lower-bound
  ([lower open]
   (make-key-range-instance (js/IDBKeyRange.lowerBound lower open)))
  ([lower]
   (make-key-range-instance (js/IDBKeyRange.lowerBound lower))))

(defn upper-bound
  ([upper open]
   (make-key-range-instance (js/IDBKeyRange.upperBound upper open)))
  ([upper]
   (make-key-range-instance (js/IDBKeyRange.upperBound upper))))

(defn only
  [value]
  (make-key-range-instance (js/IDBKeyRange.only value)))

(defn bound
  ([lower upper lowerOpen upperOpen]
   (make-key-range-instance (js/IDBKeyRange.bound lower upper lowerOpen upperOpen)))
  
  ([lower upper lowerOpen]
   (make-key-range-instance (js/IDBKeyRange.bound lower upper lowerOpen)))
  
  ([lower upper]
   (make-key-range-instance (js/IDBKeyRange.bound lower upper))))

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