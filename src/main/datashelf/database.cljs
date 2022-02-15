(ns datashelf.database
  (:require [clojure.core.async :refer [promise-chan go >! <! close!]]
            [datashelf.lang.core :refer [keep-keys to-camel-case]]
            [databox.core :as databox]))

(defn make-db-instance
  [js-db]
  {:db js-db})

(defn make-object-store-instance
  [js-object-store]
  {:object-store js-object-store})

(defn make-transaction-instance
  [js-tx]
  {:transaction js-tx})

(defn create-object-store
  [{:keys [db]} store-name options]
  {:pre [db store-name]}
  (let [js-opts (-> options
                    (keep-keys #(-> % name to-camel-case))
                    (clj->js))]
    (make-object-store-instance (.createObjectStore db store-name js-opts))))

(defn open
  [db-name version callback]
  (let [ch (promise-chan)]
    (let [open-request (.open js/indexedDB db-name (when version (long version)))]
      (set! (.-onupgradeneeded open-request)
            (fn [_]
              (println "upgrade")
              (let [db (.-result open-request)]
                (when (and callback db)
                  (callback (make-db-instance db))))))

      (set! (.-onsuccess open-request)
            (fn [_]
              (println "success")
              (let [db (.-result open-request)]
                (go
                  (>! ch (databox/success (make-db-instance db)))
                  (close! ch)))))

      (set! (.-onerror open-request)
            (fn [_]
              (println "error")
              (let [error (.-error open-request)]
                (go
                  (>! ch (databox/failure error)))))))
    ch))

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
                   (println db)))))))

(defn contains-object-store?
  [{:keys [db]} store-name]
  {:pre [db store-name]}
  (.contains (.-objectStoreNames db) store-name))

(defn transaction
  ([{:keys [db]} store-names access-mode options]
   {:pre [db
          store-names
          (if access-mode (#{"readwrite"} access-mode) true)]}
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
