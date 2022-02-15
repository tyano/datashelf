(ns datashelf.lang.core
  (:refer-clojure :exclude [group-by])
  (:require [clojure.core :as core]
            [clojure.string :as string]))

(defn group-by
  ([key-fn val-fn coll]
   (persistent!
    (reduce
     (fn [m v]
       (let [group-key (key-fn v)
             value     (val-fn v)]
         (assoc! m group-key (conj (get m group-key []) value))))
     (transient {})
     coll)))
  
  ([key-fn coll]
   (core/group-by key-fn coll)))

(defn select-by-ns
  [m ns-value]
  (let [ns-name (name ns-value)]
    (persistent!
     (reduce
      (fn [m [k v]]
        (if (= ns-name (namespace k))
          (assoc! m k v)
          m))
      (transient {})
      m))))

(defn map-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]] (assoc! m (f k) v))
    (transient {})
    m)))

(defn keep-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]] 
      (if-let [new-key (f k)]
        (assoc! m new-key v)
        m))
    (transient {})
    m)))

(defn remove-ns-part
  [v]
  {:pre [(keyword? v)]}
  (let [n (name v)]
    (keyword n)))

(defn blank?
  [str]
  (if str
    (string/blank? (string/replace str "　" ""))
    true))

(defn collection?
  [data]
  (and (coll? data) (not (map? data))))

(defn to-camel-case
  [value]
  (when value
    (let [[head & others] (string/split value "-")]
      (string/join (cons head (map string/capitalize others))))))