(ns datashelf.lang.string-list)

(defn to-vector
  [string-list]
  (when string-list
    (persistent!
     (reduce
      (fn [r idx]
        (let [item (.item string-list idx)]
          (conj! r item)))
      (transient [])
      (range 0 (.-length string-list))))))