(ns datashelf.cursor
  (:refer-clojure :exclude [update key])
  (:require [clojure.core.async :refer [promise-chan]]
            [datashelf.lang.core :as lang]
            [datashelf.request :refer [setup-request-handlers result-converter]]))

(defn make-cursor-instance
  [js-cursor]
  (when js-cursor
    {:cursor js-cursor}))

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
  ([{:keys [cursor]} {convert-result-opts :convert-result :or {convert-result-opts true}}]
   {:pre [cursor]}
   (let [v (.-value cursor)
         converter-fn (result-converter convert-result-opts)]
     (converter-fn v)))
  
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