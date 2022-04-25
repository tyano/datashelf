(ns datashelf.index
  (:refer-clojure :exclude [get count name key])
  (:require [datashelf.object-store.instance :refer [make-object-store-instance]]
            [datashelf.query :refer [Queriable]])
  (:require-macros [taoensso.timbre :refer [debug]]))

(defrecord Index [index]
  Queriable
  (js-instance [_] index))

(defn make-index-instance
  [js-index]
  (when js-index
    (->Index js-index)))

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
