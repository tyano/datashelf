(ns datashelf.core)

(defn indexed-db-supported?
  []
  (and js/window 
       (some? (aget js/window "indexedDB"))))

