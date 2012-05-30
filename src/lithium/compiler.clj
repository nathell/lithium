(ns beryl
  (:require [clojure.string :as string])
  (:use lithium))

(defn boolean? [x]
  (or (= x true) (= x false)))

(def +nil+ 2r00101111)
(def +boolean-tag+ 2r0011111)
(def wordsize 2)

(defn immediate-rep [x]
  (cond
   (integer? x) (bit-shift-left x 2)
   (nil? x)     +nil+
   (char? x)    (bit-or (bit-shift-left (int x) 8) 2r00001111)
   (boolean? x) (bit-or (if x 2r10000000 0) +boolean-tag+)))

(defn immediate? [x]
  (immediate-rep x))

(def prolog [[:mov :bp :sp]])
(def epilog [:forever [:jmp :forever]])

(defn primcall? [x]
  (list? x))

(defn third [x] (nth x 2))

(declare compile-expr)

(defn compile-let
  [bindings body si env]
  (let [[code sp env]
        (reduce
         (fn [[code sp env] [v expr]]
           [(concat code
                    (compile-expr expr sp env)
                    [[:mov [:bp sp] :ax]])
            (- sp wordsize)
            (assoc env v sp)])
         [[] si env]
         (partition 2 bindings))]
    (concat code (compile-expr body sp env))))

(defn variable? [x]
  (symbol? x))

(defn genkey []
  (-> (gensym) name string/lower-case keyword))

(defn compile-if
  [test-expr then-expr else-expr si env]
  (let [l0 (genkey) l1 (genkey)]
    (concat
     (compile-expr test-expr si env)
     [[:cmp :ax (immediate-rep false)]]
     [[:je l0]]
     [[:cmp :ax (immediate-rep nil)]]
     [[:je l0]]
     (compile-expr then-expr si env)
     [[:jmp l1]]
     [l0]
     (compile-expr else-expr si env)
     [l1])))

(defn compile-expr
  ([x] (compile-expr x (- wordsize) {}))
  ([x si env]
     (cond (immediate? x)
           [[:mov :ax (immediate-rep x)]]
           (variable? x)
           [[:mov :ax [:bp (env x)]]]
           (primcall? x)
           (condp = (first x)
               'let (compile-let (second x) (nth x 2) si env)
               'if (compile-if (second x) (nth x 2) (nth x 3) si env)
               'write-char (concat (compile-expr (second x) si env)
                                   [[:mov :ah 0x0e]
                                    [:int 0x10]])
               'byte [[:mov :al (second x)]]
               'inc (concat (compile-expr (second x) si env)
                            [[:add :ax (immediate-rep 1)]])
               '+ (concat (compile-expr (third x) si env)
                          [[:mov [:bp si] :ax]]
                          (compile-expr (second x) (- si wordsize) env)
                          [[:add :ax [:bp si]]])
               '* (concat (compile-expr (third x) si env)
                          [[:sar :ax 2]
                           [:mov [:bp si] :ax]]
                          (compile-expr (second x) (- si wordsize) env)
                          [[:mul [:bp si]]])
               '< (concat (compile-expr (third x) si env)
                          [[:mov [:bp si] :ax]]
                          (compile-expr (second x) (- si wordsize) env)
                          [[:xor :bx :bx]
                           [:cmp :ax [:bp si]]
                           [:setb :bl]
                           [:mov :ax :bx]
                           [:sal :ax 7]
                           [:or :ax +boolean-tag+]])
               'nil? (concat (compile-expr (second x) si env)
                             [[:cmp :ax +nil+]
                              [:mov :ax 0]
                              [:sete :al]
                              [:sal :ax 7]
                              [:or :ax +boolean-tag+]])
               'do (apply concat (map #(compile-expr % si env) (rest x)))))))

(defn compile-program [x]
  (concat prolog (compile-expr x) epilog))

(defn os []
  (compile-program
   '(do
      (write-char (byte 72))
      (write-char (byte 101))
      (write-char (byte 108))
      (write-char (byte 108))
      (write-char (byte 111))
      )))