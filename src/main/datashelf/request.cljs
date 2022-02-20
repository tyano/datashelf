(ns datashelf.request
  (:require [clojure.core.async :refer [>! close! go]]
            [databox.core :as databox]))

(defn setup-request-handlers
  [request ch & [result-fn]]
  (set! (.-onsuccess request)
        (fn [e]
          (let [result (.-result request)
                result (if result-fn (result-fn result) result)]
            (go
              (>! ch (databox/success result))
              (close! ch)))))

  (set! (.-onerror request)
        (fn [_]
          (let [error (.-error request)]
            (go
              (>! ch (databox/failure error))
              (close! ch))))))