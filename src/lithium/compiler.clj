(ns lithium.compiler
  (:require [clojure.string :as string])
  (:use lithium.assembler))

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

(def primitives {})

(defmacro defprimitive [name args & code]
  (let [code (partition-by vector? code)]
    `(def primitives
       (assoc primitives '~name
              (fn [~'si ~'env [_# ~@args]]
                (concat
                 ~@(apply concat
                          (for [x code]
                            (if (vector? (first x))
                              (vector (vec x))
                              x)))))))))

(defprimitive + [a b]
  (compile-expr b si env)
  [:mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env)
  [:add :ax [:bp si]])

(defprimitive * [a b]
  (compile-expr b si env)
  [:sar :ax 2]
  [:mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env)
  [:mul [:bp si]])

(defprimitive < [a b]
  (compile-expr b si env)
  [:mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env)
  [:xor :bx :bx]
  [:cmp :ax [:bp si]]
  [:setb :bl]
  [:mov :ax :bx]
  [:sal :ax 7]
  [:or :ax +boolean-tag+])

(defprimitive write-char [x]
  (compile-expr x si env)
  [:mov :ah 0x0e]
  [:int 0x10])

(defprimitive byte [x]
  [:mov :al x])

(defprimitive inc [x]
  (compile-expr x si env)
  [:add :ax (immediate-rep 1)])

(defprimitive nil? [x]
  (compile-expr (second x) si env)
  [:cmp :ax +nil+]
  [:mov :ax 0]
  [:sete :al]
  [:sal :ax 7]
  [:or :ax +boolean-tag+])

(defn compile-expr
  ([x] (compile-expr x (- wordsize) {}))
  ([x si env]
     (cond (immediate? x)
           [[:mov :ax (immediate-rep x)]]
           (variable? x)
           [[:mov :ax [:bp (env x)]]]
           (primcall? x)
           (if-let [prim (primitives (first x))]
             (prim si env x)
             (condp = (first x)
               'let (compile-let (second x) (nth x 2) si env)
               'if (compile-if (second x) (nth x 2) (nth x 3) si env)
               'do (apply concat (map #(compile-expr % si env) (rest x)))
               (throw (Exception. (format "Unknown primitive: %s" (first x)))))))))

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