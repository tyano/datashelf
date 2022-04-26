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


(defn convert-value
  [v convert-options]
  (if convert-options
    (if (boolean? convert-options)
      (if (true? convert-options) (clj->js v) v)
      (clj->js v convert-options))
    v))

(defn result-converter
  [convert-options]
  (when convert-options
    (if (boolean? convert-options)
      (when (true? convert-options) js->clj)
      #(js->clj % convert-options))))