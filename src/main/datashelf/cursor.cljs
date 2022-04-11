(ns datashelf.cursor
  (:refer-clojure :exclude [update key])
  (:require [clojure.core.async :refer [promise-chan]]
            [datashelf.lang.core :as lang]
            [datashelf.request :refer [setup-request-handlers]]))

(defn make-cursor-instance
  [js-cursor]
  {:cursor js-cursor})

(defn source
  [{:keys [cursor]}]
  {:pre [cursor]}
  (.-source cursor))

(defn direction
  [{:keys [cursor]}]
  {:pre [cursor]}
  (some-> (.-direction cursor)
          (keyword)))

(defn key
  [{:keys [cursor]}]
  {:pre [cursor]}
  (.-key cursor))

(defn primary-key
  [{:keys [cursor]}]
  {:pre [cursor]}
  (.-primaryKey cursor))

(defn request
  [{:keys [cursor]}]
  {:pre [cursor]}
  (.-request cursor))

(defn value
  ([{:keys [cursor]} {:keys [convert-result] :or [convert-result true]}]
   {:pre [cursor]}
   (let [v (.-value cursor)]
     (if convert-result
       (if (boolean? convert-result)
         (if (true? convert-result) (lang/js->clj v) v)
         (lang/js->clj v convert-result))
       v)))
  
  ([instance]
   (value instance nil)))

(defn advance
  [{:keys [cursor] :as instance} count]
  {:pre [cursor count]}
  (.advance cursor count)
  instance)

(defn continue
  ([{:keys [cursor] :as instance} key]
   {:pre [cursor]}
   (if (some? key)
     (.continue cursor key)
     (.continue cursor))
   instance)
  
  ([instance]
   (continue instance nil)))

(defn continue-primary-key
  [{:keys [cursor] :as instance} key primary-key]
  {:pre [cursor key primary-key]}
  (.continuePrimaryKey cursor key primary-key)
  instance)

(defn delete
  [{:keys [cursor] :as instance}]
  {:pre [cursor]}
  (.delete cursor)
  instance)

(defn update
  [{:keys [cursor]} value options]
  {:pre [cursor]}
  (let [ch (promise-chan)
        data    (lang/clj->js value options)
        request (.update cursor data)]
    (setup-request-handlers request ch)
    ch))