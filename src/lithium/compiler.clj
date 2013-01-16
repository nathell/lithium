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
            (fn [~'si ~'env ~'rp [_# ~@args]]
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

(def prolog [['cli]
             ['mov :bp :sp]
             ['mov :si :heap-start]
             ['add :si 7]
             ['and :si 0xfff8]])
(def epilog [:forever ['jmp :forever] :heap-start])

(defn primcall? [x]
  (or (list? x) (seq? x)))

(defn third [x] (nth x 2))

(declare compile-expr)

(defn make-environment-element
  [symbol type offset]
  [symbol {:type type, :offset offset}])

(defn compile-let
  [bindings body si env loop? rp]
  (let [[code sp env]
        (reduce
         (fn [[code sp env] [v expr]]
           [(codeseq
             code
             (compile-expr expr sp env rp)
             ['mov [:bp sp] :ax])
            (- sp 2)
            (conj env (make-environment-element v :bound sp))])
         [[] si env]
         (partition 2 bindings))]
    (concat code (when loop? [rp])
            (map #(compile-expr % sp env rp) body))))

(defn variable? [x]
  (symbol? x))

(defn genkey []
  (-> (gensym) name string/lower-case keyword))

(defn compile-if
  [test-expr then-expr else-expr si env rp]
  (let [l0 (genkey) l1 (genkey)]
    (codeseq
     (compile-expr test-expr si env rp)
     ['cmp :ax (immediate-rep false)]
     ['je l0]
     ['cmp :ax (immediate-rep nil)]
     ['je l0]
     (compile-expr then-expr si env rp)
     ['jmp l1]
     l0
     (compile-expr else-expr si env rp)
     l1)))

(defn compile-call
  [si env expr args rp]
  (codeseq
   ['sub :sp (* wordsize (+ 2 (count env)))]
   (map-indexed
    (fn [i expr]
      (codeseq
       (compile-expr expr (- si (* wordsize (inc i))) env rp)
       ['push :ax]))
    args)
   (compile-expr expr (- si (* wordsize (inc (count args)))) env rp)
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
  [si env labels code rp]
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
          (compile-expr body (- si (* wordsize (count args))) (into env (concat arg-env fvar-env)) rp))
        ['add :bp wordsize]
        ['ret]))
     body-label
     (compile-expr code si env rp))))

(defprimitive + [a b]
  (compile-expr b si env rp)
  ['mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env rp)
  ['add :ax [:bp si]])

(defprimitive * [a b]
  (compile-expr b si env rp)
  ['sar :ax 2]
  ['mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env rp)
  ['mul [:bp si]])

(defprimitive < [a b]
  (compile-expr b si env rp)
  ['mov [:bp si] :ax]
  (compile-expr a (- si wordsize) env rp)
  ['xor :bx :bx]
  ['cmp :ax [:bp si]]
  ['setb :bl]
  ['mov :ax :bx]
  ['sal :ax 7]
  ['or :ax +boolean-tag+])

(defprimitive write-char [x]
  (compile-expr x si env rp)
  ['mov :ah 0x0e]
  ['int 0x10])

(defprimitive byte [x]
  ['mov :al x])

(defprimitive inc [x]
  (compile-expr x si env rp)
  ['add :ax (immediate-rep 1)])

(defprimitive nil? [x]
  (compile-expr (second x) si env rp)
  ['cmp :ax +nil+]
  ['mov :ax 0]
  ['sete :al]
  ['sal :ax 7]
  ['or :ax +boolean-tag+])

(defprimitive cons [x y]
  (compile-expr x si env rp)
  ['mov [:si] :ax]
  (compile-expr y si env rp)
  ['mov [:si wordsize] :ax]
  ['mov :ax :si]
  ['or :ax +cons-tag+]
  ['add :si 8])

(defprimitive car [x]
  (compile-expr x si env rp)
  ['mov :bx :ax]
  ['mov :ax [:bx -1]])

(defprimitive recur [& exprs]
  (for [[i expr] (map-indexed vector exprs)]
    [(compile-expr expr (- si (* i wordsize)) env rp)
     ['mov [:bp (- si (* i wordsize))] :ax]])
  (for [i (range (count exprs))]
    [['mov :bx [:bp (- si (* i wordsize))]]
     ['mov [:bp (- (* (inc i) wordsize))] :bx]])
  ['jmp rp])

(defprimitive cdr [x]
  (compile-expr x si env rp)
  ['mov :bx :ax]
  ['mov :ax [:bx (dec wordsize)]])

(defprimitive closure [label free-vars]
  ['mov [:si] (-> label name keyword)]
  ['add :si wordsize]
  (for [fvar free-vars]
    [(compile-expr fvar si env rp)
     ['mov [:si] :ax]
     ['add :si wordsize]])
  ['mov :ax :si]
  ['sub :ax (* wordsize (inc (count free-vars)))]
  ['or :ax +closure-tag+]
  ['add :si 7]
  ['and :si 0xfff8])

(defn compile-expr
  ([x] (compile-expr x (- wordsize) {} nil))
  ([x si env rp]
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
             (prim si env rp x)
             (condp = (first x)
               'let (compile-let (second x) (next (next x)) si env false rp)
               'loop (compile-let (second x) (next (next x)) si env true (genkey))
               'if (compile-if (second x) (nth x 2) (nth x 3) si env rp)
               'do (apply concat (map #(compile-expr % si env rp) (rest x)))
               'fncall (compile-call si env (second x) (next (next x)) rp)
               'labels (compile-labels si env (second x) (nth x 2) rp)
               (throw (Exception. (format "Unknown primitive: %s" (first x)))))))))

(defn collect-closures
  [bound-vars expr]
  (cond
   (not (primcall? expr))
   [[] expr]
   
   (#{'let 'loop} (first expr))
   (let [[bound-vars binding-closures bindings]
         (reduce (fn [[bound-vars closures bindings] [sym val-expr]]
                   (let [[new-closures new-val-expr] (collect-closures bound-vars val-expr)]
                     [(conj bound-vars sym) (into closures new-closures) (conj bindings sym new-val-expr)]))
                 [bound-vars [] []]
                 (partition 2 (second expr)))
         transformed-body (map (partial collect-closures bound-vars) (next (next expr)))
         body-closures (apply concat (map first transformed-body))
         new-body (map second transformed-body)]
     [(into binding-closures body-closures)
      `(~(first expr) ~(vec bindings) ~@new-body)])

   (or (#{'if 'do 'recur} (first expr)) (primitives (first expr)))
   (let [transformed-body (map (partial collect-closures bound-vars) (next expr))]
     [(apply concat (map first transformed-body))
      `(~(first expr) ~@(map second transformed-body))])

   (= (first expr) 'fn)
   (let [body-bound-vars (into bound-vars (second expr))
         transformed-body (map (partial collect-closures body-bound-vars) (next (next expr)))
         sym (gensym "cl")]
     [(conj (vec (apply concat (map first transformed-body)))
            `(~sym (~'code ~(second expr) ~(vec bound-vars) ~@(map second transformed-body))))
      `(~'closure ~sym ~(vec bound-vars))])

   :otherwise
   (let [transformed-fn (collect-closures bound-vars (first expr))
         transformed-body (map (partial collect-closures bound-vars) (next expr))]
     [(apply concat (first transformed-fn) (map first transformed-body))
      `(~'fncall ~(second transformed-fn) ~@(map second transformed-body))])))

(defn clojure->tcr
  [prog]
  (let [[closures expr] (collect-closures #{} prog)]
    `(~'labels ~(vec (apply concat closures)) ~expr)))

(defn compile-program [x]
  (concat prolog (compile-expr (clojure->tcr x)) epilog))