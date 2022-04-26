(ns datashelf.lang.core
  (:refer-clojure :exclude [group-by js->clj clj->js])
  (:require [clojure.core :as core]
            [clojure.string :as string]))

(defn map-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]] (assoc! m (f k) v))
    (transient {})
    m)))

(defn keep-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]] 
      (if-let [new-key (f k)]
        (assoc! m new-key v)
        m))
    (transient {})
    m)))

(defn to-camel-case
  [value]
  (when value
    (let [[head & others] (string/split value "-")]
      (string/join (cons head (map string/capitalize others))))))

(def ^:private upper-chars #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z})

(defn- upper?
  [c]
  (boolean (upper-chars c)))

(defn to-kebab-case
  [value]
  (when value
    (->> (reduce
          (fn [result c]
            (if (upper? (char c))
              (str result "-" c)
              (str result c)))
          ""
          (seq value))
         (string/lower-case))))

(defn flatten-map
  [m]
  (-> m seq flatten))

(defn map-keys-recursive
  [m f]
  (letfn [(process-value
            [v]
            (cond
              (map? v)
              (map-keys-recursive v f)

              (sequential? v)
              (map process-value v)

              :else
              v))]
    (persistent!
     (reduce
      (fn [m [k v]] (assoc! m (f k) (process-value v)))
      (transient {})
      m))))

(def ^:dynamic *default-js->clj-options* {:kebabcasify-keys true :keywordize-keys true})

(defn set-default-js->clj-options!
  [options]
  (set! *default-js->clj-options* options))

(defn js->clj
  [v & [options]]
  (let [{:keys [kebabcasify-keys keywordize-keys] :as options} (merge *default-js->clj-options* options)]
    (letfn [(postprocess-key
              [k]
              (let [keyname (if kebabcasify-keys
                              (-> k name to-kebab-case)
                              (-> k name))]
                (if keywordize-keys
                  (keyword keyname)
                  keyname)))

            (postprocess
              [v]
              (cond
                (map? v)
                (map-keys-recursive v postprocess-key)

                (sequential? v)
                (map postprocess v)

                :else
                v))]

      (let [converted (apply core/js->clj v (flatten-map (dissoc options :kebabcasify-keys :keywordize-keys)))]
        (postprocess converted)))))



(def ^:dynamic *default-clj->js-options* {:camelcasify-keys true})

(defn set-default-clj->js-options!
  [options]
  (set! *default-clj->js-options* options))

(defn clj->js
  [v & [options]]
  (let [{:keys [camelcasify-keys] :as options} (merge *default-clj->js-options* options)]
    (letfn [(preprocess
              [v]
              (cond
                (and (map? v) camelcasify-keys)
                (map-keys-recursive v #(-> % name to-camel-case))

                (and (sequential? v) camelcasify-keys)
                (map preprocess v)

                :else
                v))]
      (let [preprocessed (preprocess v)]
        (apply core/clj->js preprocessed (flatten-map (dissoc options :camelcasify-keys)))))))
