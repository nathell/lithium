(ns lithium.compiler.repr)

;;; fixed-size values

;; last bit set:   numbers (-16384..16383)
;; last bit unset: 0 – nil, 2 – false, 4 – true, 6..516 – char, 518+ – pointer to heap

(def wordsize 2)
(def min-heap-start 518)

(defn immediate [x]
  (cond
    (integer? x) (inc (bit-shift-left x 1))
    (nil? x)     0
    (boolean? x) (if x 4 2)
    (char? x)    (+ 6 (bit-shift-left (int x) 1))))

(defn immediate? [x]
  (immediate x))

;;; heap

(def cons-typeid 0)

;; strings, symbols, keywords: 1-byte typeid followed by 1-bit length

(def string-typeid 1)
(def symbol-typeid 2)
(def keyword-typeid 3)

;; vectors: 2-byte typeid followed by 2-bit length (in immediate repr)

(def vector-typeid 4)
