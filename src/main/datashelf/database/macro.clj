(ns datashelf.database.macro)

(defmacro upgrade-db
  [old-version & body]
  {:pre [old-version (zero? (rem (count body) 2))]}
  (let [kv-coll (->> (partition 2 body)
                     (map (fn [[k v]] (vector `(js/Number ~k) `(fn [] ~v)))))]
    `(let [fns# ~(into (hash-map) kv-coll)]
       (datashelf.database/upgrade-db-fn ~old-version fns#))))