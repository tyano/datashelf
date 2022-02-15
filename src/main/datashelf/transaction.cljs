(ns datashelf.transaction
  (:require [datashelf.database :refer [make-db-instance make-object-store-instance]]
            [datashelf.lang.string-list :refer [to-vector]]))

(defn db
  [{:keys [transaction]}]
  {:pre [transaction]}
  (make-db-instance (.-db transaction)))

(defn durability
  [{:keys [transaction]}]
  {:pre [transaction]}
  (.-durability transaction))

(defn error
  [{:keys [transaction]}]
  {:pre [transaction]}
  (.-error transaction))

(defn mode
  [{:keys [transaction]}]
  {:pre [transaction]}
  (.-mode transaction))

(defn object-store-names
  [{:keys [transaction]}]
  {:pre [transaction]}
  (to-vector (.-objectStoreNames transaction)))

(defn object-store
  [{:keys [transaction]} store-name]
  {:pre [transaction store-name]}
  (make-object-store-instance (.objectStore transaction store-name)))

(defn abort
  [{:keys [transaction]}]
  {:pre [transaction]}
  (.abort transaction))

(defn commit
  [{:keys [transaction]}]
  {:pre [transaction]}
  (.commit transaction))

(defn onabort
  [{:keys [transaction] :as tx} abort-fn]
  {:pre [transaction abort-fn (fn? abort-fn)]}
  (set! (.-onabort transaction) (fn [e] (abort-fn e tx)))
  tx)

(defn oncomplete
  [{:keys [transaction] :as tx} complete-fn]
  {:pre [transaction complete-fn (fn? complete-fn)]}
  (set! (.-oncomplete transaction) (fn [e] (complete-fn e tx)))
  tx)

(defn onerror
  [{:keys [transaction] :as tx} error-fn]
  {:pre [transaction error-fn (fn? error-fn)]}
  (set! (.-onabort transaction) (fn [e] (error-fn e tx)))
  tx)