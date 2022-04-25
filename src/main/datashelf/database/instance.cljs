(ns datashelf.database.instance)

(defn make-db-instance
  [js-db]
  (when js-db {:db js-db}))