(def write-string
  (fn [s]
    (let [len (strlen s)]
      (loop [i 0]
        (if (= i len)
          nil
          (do
            (write-char (char-at s i))
            (recur (inc i))))))))

(write-string "Life")
(write-string " is ")
(write-string "life\r\n")
(write-string "NaNaNaNaNa!\r\n")
