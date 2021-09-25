(ns lithium.compiler.repr)

(def nil-tag 2r00101111)
(def boolean-tag 2r0011111)
(def cons-tag 2r001)
(def closure-tag 2r110)
(def wordsize 2)

(defn immediate [x]
  (cond
   (integer? x) (bit-shift-left x 2)
   (nil? x)     nil-tag
   (char? x)    (bit-or (bit-shift-left (int x) 8) 2r00001111)
   (boolean? x) (bit-or (if x 2r10000000 0) boolean-tag)))

(defn immediate? [x]
  (immediate x))
