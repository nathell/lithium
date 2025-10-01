#_(comment
  (def write-char print)
  (def int->char char)
  (def strlen count)
  (def vector-length count)
  (def vector-at nth)
  (def char-at nth))

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

(def print-str
  (fn [s]
    (let [cnt (strlen s)]
      (loop [i 0]
        (if (< i cnt)
          (do
            (write-char (char-at s i))
            (recur (+ i 1)))
          nil)))))

(def xprint nil)

(def xprint
  (fn [x]
    (if (string? x)
      (do
        (write-char \")
        (print-str x)
        (write-char \"))
      (if (number? x)
        (write-number x)
        (if (boolean? x)
          (print-str (if x "true" "false"))
          (if (vector? x)
            (do
              (write-char \[)
              (let [cnt (vector-length x)]
                (loop [i 0]
                  (xprint (vector-at x i))
                  (if (< i (- cnt 1))
                    (write-char \space)
                    nil)
                  (if (< i cnt)
                    (recur (+ i 1))
                    nil)))
              (write-char \]))
            (print-str "[unknown]")))))))

#_(def print-mul
    (fn [a b]
      (let []
        (write-number a)
        (write-char \*)
        (write-number b)
        (write-char \=)
        (write-number (* a b)))))

#_(print-str (if (number? 5) "yes\r\n" "no\r\n"))

(xprint [42 "dupa" true])


#_(print-mul 23 79)
