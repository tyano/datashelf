(ns datashelf.transaction
  (:require [datashelf.database.instance :refer [make-db-instance]]
            [datashelf.lang.string-list :refer [to-vector]]
            [datashelf.object-store.instance :refer [make-object-store-instance]]
            [clojure.core.async :refer [go >! chan close!]]
            [databox.core :as databox]))

(defn make-transaction-instance
  [js-tx]
  (when js-tx {:transaction js-tx}))

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
  (set! (.-onerror transaction) (fn [e] (error-fn e tx)))
  tx)

(defn open-event-channel
  [tx]
  (let [tx-ch (chan)]
    (oncomplete tx (fn [e _] (go (>! tx-ch (databox/success {:type :completed :event e})) (close! tx-ch))))
    (onabort tx (fn [e _] (go (>! tx-ch (databox/failure {:type :aborted :event e})) (close! tx-ch))))
    (onerror tx (fn [e _] (go (>! tx-ch (databox/failure {:type :error :event e})) (close! tx-ch))))
    tx-ch))