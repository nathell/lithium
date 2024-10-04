#_(comment
  (def write-char print)
  (def int->char char))

(def write-number
  (fn [x]
    (if (= x 0)
      (write-char \0)
      (loop [modulus 1000 n x emitting? false]
        (if (= modulus 0)
          nil
          (let [digit (quot n modulus)
                now-emitting? (if emitting? true (< 0 digit))]
            (if now-emitting?
              (write-char (int->char (+ 48 digit)))
              nil)
            (recur (quot modulus 10)
                   (- n (* digit modulus))
                   now-emitting?)))))))

(def print-mul
  (fn [a b]
    (let []
      (write-number a)
      (write-char \*)
      (write-number b)
      (write-char \=)
      (write-number (* a b)))))

(print-mul 23 79)
