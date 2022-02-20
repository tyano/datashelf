(ns datashelf.object-store.instance)

(defn make-index-instance
  [js-index]
  {:index js-index})

(defn make-object-store-instance
  [js-object-store]
  {:object-store js-object-store})