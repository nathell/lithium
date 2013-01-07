(ns lithium.compiler
  (:require [clojure.string :as string])
  (:use lithium.assembler))

(def primitives {})

(defn instr? [x]
  (or (not (sequential? x)) (symbol? (first x))))

(defn codeseq [& code]
  (filter instr? (rest (tree-seq (complement instr?) seq code))))

(defmacro defprimitive [name args & code]
  `(def primitives
     (assoc primitives '~name
            (fn [~'si ~'env [_# ~@args]]
              (codeseq ~@code)))))

(defn boolean? [x]
  (or (= x true) (= x false)))

(def +nil+ 2r00101111)
(def +boolean-tag+ 2r0011111)
(def +cons-tag+ 2r001)
(def +closure-tag+ 2r110)
(def wordsize 2)

(defn immediate-rep [x]
  (cond
   (integer? x) (bit-shift-left x 2)
   (nil? x)     +nil+
   (char? x)    (bit-or (bit-shift-left (int x) 8) 2r00001111)
   (boolean? x) (bit-or (if x 2r10000000 0) +boolean-tag+)))

(defn immediate? [x]
  (immediate-rep x))

(def prolog [['mov :bp :sp]
             ['mov :si :heap-start]
             ['add :si 7]
             ['and :si 0xfff8]])
(def epilog [:forever ['jmp :forever] :heap-start])

(defn primcall? [x]
  (list? x))

(defn third [x] (nth x 2))

(declare compile-expr)

(defn make-environment-element
  [symbol type offset]
  [symbol {:type type, :offset offset}])

(defn compile-let
  [bindings body si env]
  (let [[code sp env]
        (reduce
         (fn [[code sp env] [v expr]]
           [(codeseq
             code
             (compile-expr expr sp env)
             ['mov [:bp sp] :ax])
            (- sp 2)
            (conj env (make-environment-element v :bound sp))])
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
    (codeseq
     (compile-expr test-expr si env)
     ['cmp :ax (immediate-rep false)]
     ['je l0]
     ['cmp :ax (immediate-rep nil)]
     ['je l0]
     (compile-expr then-expr si env)
     ['jmp l1]
     l0
     (compile-expr else-expr si env)
     l1)))

(defn compile-call
  [si env expr args]
  (codeseq
   ['sub :sp (* wordsize (+ 2 (count env)))]
   (map-indexed
    (fn [i expr]
      (codeseq
       (compile-expr expr (- si (* wordsize (inc i))) env)
       ['push :ax]))
    args)
   (compile-expr expr (- si (* wordsize (inc (count args)))) env)
   ['add :sp (* wordsize (+ 2 (count args)))]
   ['push :di]
   ['mov :di :ax]
   ['mov :bx :ax]
   ['mov :bp :sp]
   ['call [:bx (- +closure-tag+)]]
   ['pop :di]
   ['add :sp (* wordsize (count env))]
   ['mov :bp :sp]))

(defn compile-labels
  [si env labels code]
  (let [labels (partition 2 labels)
        body-label (genkey)]
    (codeseq
     ['jmp body-label]
     (for [[label [code-sym args fvars body]] labels]
       (codeseq
        (keyword (name label))
        ['sub :bp wordsize]
        (let [arg-env (map-indexed (fn [i x]
                                     (make-environment-element x :bound (- -2 i i))
                                     #_[x (* (- -1 i) wordsize)]) args)
              fvar-env (map-indexed (fn [i x]
                                      (make-environment-element x :free (+ 2 i i))) fvars)]
          (compile-expr body (- si (* wordsize (count args))) (into env (concat arg-env fvar-env))))
        ['add :bp wordsize]
        ['ret]))
     body-label
     (compile-expr code si env))))

(defprimitive + [a b]
  (compile-expr b si env)
  ['mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env)
  ['add :ax [:bp si]])

(defprimitive * [a b]
  (compile-expr b si env)
  ['sar :ax 2]
  ['mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env)
  ['mul [:bp si]])

(defprimitive < [a b]
  (compile-expr b si env)
  ['mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env)
  ['xor :bx :bx]
  ['cmp :ax [:bp si]]
  ['setb :bl]
  ['mov :ax :bx]
  ['sal :ax 7]
  ['or :ax +boolean-tag+])

(defprimitive write-char [x]
  (compile-expr x si env)
  ['mov :ah 0x0e]
  ['int 0x10])

(defprimitive byte [x]
  ['mov :al x])

(defprimitive inc [x]
  (compile-expr x si env)
  ['add :ax (immediate-rep 1)])

(defprimitive nil? [x]
  (compile-expr (second x) si env)
  ['cmp :ax +nil+]
  ['mov :ax 0]
  ['sete :al]
  ['sal :ax 7]
  ['or :ax +boolean-tag+])

(defprimitive cons [x y]
  (compile-expr x si env)
  ['mov [:si] :ax]
  (compile-expr y si env)
  ['mov [:si wordsize] :ax]
  ['mov :ax :si]
  ['or :ax +cons-tag+]
  ['add :si 8])

(defprimitive car [x]
  (compile-expr x si env)
  ['mov :bx :ax]
  ['mov :ax [:bx -1]])

(defprimitive cdr [x]
  (compile-expr x si env)
  ['mov :bx :ax]
  ['mov :ax [:bx (dec wordsize)]])

(defprimitive closure [label free-vars]
  ['mov [:si] (-> label name keyword)]
  ['add :si wordsize]
  (for [fvar free-vars]
    [(compile-expr fvar si env)
     ['mov [:si] :ax]
     ['add :si wordsize]])
  ['mov :ax :si]
  ['sub :ax (* wordsize (inc (count free-vars)))]
  ['or :ax +closure-tag+]
  ['add :si 7]
  ['and :si 0xfff8])

(defn compile-expr
  ([x] (compile-expr x (- wordsize) {}))
  ([x si env]
     (cond (immediate? x)
           [['mov :ax (immediate-rep x)]]
           (variable? x)
           (let [{:keys [type offset]} (env x)]
             (if (= type :bound)
               [['mov :ax [:bp offset]]]
               [['mov :bx :di]
                ['mov :ax [:bx (- offset +closure-tag+)]]]))
           (primcall? x)
           (if-let [prim (primitives (first x))]
             (prim si env x)
             (condp = (first x)
               'let (compile-let (second x) (nth x 2) si env)
               'if (compile-if (second x) (nth x 2) (nth x 3) si env)
               'do (apply concat (map #(compile-expr % si env) (rest x)))
               'fncall (compile-call si env (second x) (next (next x)))
               'labels (compile-labels si env (second x) (nth x 2))
               (throw (Exception. (format "Unknown primitive: %s" (first x)))))))))

(defn compile-program [x]
  (concat prolog (compile-expr x) epilog))