(ns datashelf.object-store.instance
  (:require [datashelf.query :refer [Queriable]]))

(defrecord ObjectStore [object-store]
  Queriable
  (js-instance [_] object-store))

(defn make-object-store-instance
  [js-object-store]
  (when js-object-store
    (->ObjectStore js-object-store)))