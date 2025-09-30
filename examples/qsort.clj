;; for this, we need: vectors, first, next, filter, concat

(defn qsort [xs]
  (if (nil? xs)
    nil
    (if (nil? (next xs))
      xs
      (let [pivot (first xs)
            remainder (next xs)
            smaller (filter (fn [x] (< x pivot)) remainder)
            larger (filter (fn [x] (>= x pivot)) remainder)]
        (concat (concat (qsort smaller) [pivot]) (qsort larger))))))
