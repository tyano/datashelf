(ns datashelf.core
  (:require [clojure.core.async :refer [<! >! go go-loop]]
            [datashelf.database :as db]))

(defn main
  []
  (println "test"))

(defn indexed-db-supported?
  []
  (and js/window 
       (some? (aget js/window "indexedDB"))))

