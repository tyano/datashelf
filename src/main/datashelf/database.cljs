(ns datashelf.database
  (:require [clojure.core.async :refer [>! close! go chan]]
            [databox.core :as databox]
            [datashelf.lang.core :refer [keep-keys to-camel-case]]
            [datashelf.object-store.instance :refer [make-object-store-instance]]
            [datashelf.transaction :refer [make-transaction-instance]]
            [datashelf.database.instance :refer [make-db-instance]]
            [taoensso.timbre :refer-macros [debug] :as timbre]))

(defn create-object-store
  [{:keys [db]} store-name options]
  {:pre [db store-name]}
  (let [js-opts (-> options
                    (keep-keys #(-> % name to-camel-case))
                    (clj->js))]
    (make-object-store-instance (.createObjectStore db store-name js-opts))))

(defn open
  ([db-name version callback {:keys [output-chan]}]
   (let [ch (or output-chan (chan 1))
         open-request (.open js/indexedDB db-name (when version (long version)))]
     (set! (.-onupgradeneeded open-request)
           (fn [e]
             (let [old-version (.-oldVersion e)
                   new-version (.-newVersion e)
                   tx (.-transaction open-request)
                   db (.-result open-request)]
               (js/console.log tx)
               (when (and callback db)
                 (callback (make-db-instance db) {:old-version old-version
                                                  :new-version new-version
                                                  :transaction (make-transaction-instance tx)
                                                  :event e})))))

     (set! (.-onsuccess open-request)
           (fn [_]
             (debug "success")
             (let [db (.-result open-request)]
               (go
                 (>! ch (databox/success (make-db-instance db)))
                 (close! ch)))))

     (set! (.-onerror open-request)
           (fn [_]
             (debug "error")
             (let [error (.-error open-request)]
               (go
                 (>! ch (databox/failure error))
                 (close! ch)))))

     ch))

  ([db-name version callback]
   (open db-name version callback nil)))

(defn upgrade-db-fn
  [old-version fns]
  (doseq [[version f] (into (sorted-map) fns)]
    (when (< old-version version)
      (debug "upgrade db to version" version)
      (f))))

(defn delete-all-databases
  []
  (-> js/indexedDB
      (.databases)
      (.then (fn [r]
               (doseq [i (range 0 (alength r))]
                 (let [db (aget r i)
                       db-name (.-name db)]
                   (.deleteDatabase js/indexedDB db-name)))))))

(defn show-all-databases
  []
  (-> js/indexedDB
      (.databases)
      (.then (fn [r]
               (doseq [i (range 0 (alength r))]
                 (let [db (aget r i)
                       db-name (.-name db)]
                   (debug db)))))))

(defn contains-object-store?
  [{:keys [db]} store-name]
  {:pre [db store-name]}
  (.contains (.-objectStoreNames db) store-name))

(defn transaction
  ([{:keys [db]} store-names access-mode options]
   {:pre [db
          store-names
          (if access-mode (#{"readwrite" "readonly"} access-mode) true)]}
   (make-transaction-instance
    (if (seq options)
      (let [js-opts (-> options
                        (keep-keys #(-> % name to-camel-case))
                        (clj->js))]
        (.transaction db store-names access-mode js-opts))
      (.transaction db store-names access-mode))))

  ([db-instance store-names access-mode]
   (transaction db-instance store-names access-mode nil))

  ([db-instance store-names]
   (transaction db-instance store-names nil)))
