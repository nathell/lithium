(ns lithium.compiler
  (:require [clojure.string :as string])
  (:use [lithium.utils :only [read-all update]]))

(def primitives {})

(defn instr? [x]
  (or (not (sequential? x)) (symbol? (first x))))

(defn codeseq [& code]
  (filter instr? (rest (tree-seq (complement instr?) seq code))))

(defmacro defprimitive [name args & code]
  `(def primitives
     (assoc primitives '~name
            (fn [~'state [_# ~@args]]
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
  [bindings body loop? state]
  (let [[code state]
        (reduce
         (fn [[code {:keys [stack-pointer environment] :as state}] [v expr]]
           [(codeseq
             code
             (compile-expr expr state)
             ['mov [:bp stack-pointer] :ax])
            (update state
                    :stack-pointer (- 2)
                    :environment (conj (make-environment-element v :bound stack-pointer)))])
         [[] state]
         (partition 2 bindings))]
    (concat code 
            (when loop? [(:recur-point state)])
            (mapcat #(compile-expr % state) body))))

(defn variable? [x]
  (symbol? x))

(defn genkey []
  (-> (gensym) name string/lower-case keyword))

(defn compile-if
  [test-expr then-expr else-expr state]
  (let [l0 (genkey) l1 (genkey)]
    (codeseq
     (compile-expr test-expr state)
     ['cmp :ax (immediate-rep false)]
     ['je l0]
     ['cmp :ax (immediate-rep nil)]
     ['je l0]
     (compile-expr then-expr state)
     ['jmp l1]
     l0
     (compile-expr else-expr state)
     l1)))

(defn compile-call
  [expr args {:keys [environment] :as state}]
  (codeseq
   ['sub :sp (* wordsize (+ 2 (count environment)))]
   (map-indexed
    (fn [i expr]
      (codeseq
       (compile-expr expr (update state :stack-pointer (- (* wordsize (inc i)))))
       ['push :ax]))
    args)
   (compile-expr expr (update state :stack-pointer (- (* wordsize (inc (count args))))))
   ['add :sp (* wordsize (+ 2 (count args)))]
   ['push :di]
   ['mov :di :ax]
   ['mov :bx :ax]
   ['mov :bp :sp]
   ['call [:bx (- +closure-tag+)]]
   ['pop :di]
   ['add :sp (* wordsize (count environment))]
   ['mov :bp :sp]))

(defn compile-labels
  [labels code state]
  (let [labels (partition 2 labels)
        body-label (genkey)]
    (codeseq
     ['jmp body-label]
     (for [[label [code-sym args fvars body]] labels]
       (codeseq
        (keyword (name label))
        ['sub :bp wordsize]
        (let [arg-env (map-indexed (fn [i x]
                                     (make-environment-element x :bound (- -2 i i))) args)
              fvar-env (map-indexed (fn [i x]
                                      (make-environment-element x :free (+ 2 i i))) fvars)]
          (compile-expr body 
                        (update state
                                :stack-pointer (- (* wordsize (count args)))
                                :environment (into (concat arg-env fvar-env)))))
        ['add :bp wordsize]
        ['ret]))
     body-label
     (compile-expr code state))))

(defprimitive + [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer (- wordsize)))
  ['add :ax [:bp (:stack-pointer state)]])

(defprimitive - [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer (- wordsize)))
  ['sub :ax [:bp (:stack-pointer state)]])

(defprimitive * [a b]
  (compile-expr b state)
  ['sar :ax 2]
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer (- wordsize)))
  ['mul [:word :bp (:stack-pointer state)]])

(defprimitive mod [a b]
  (compile-expr b state)
  ['sar :ax 2]
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer (- wordsize)))
  ['mov :dx 0]
  ['sar :ax 2]
  ['div [:word :bp (:stack-pointer state)]]
  ['mov :ax :dx]
  ['sal :ax 2])

(defprimitive = [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer (- wordsize)))
  ['cmp :ax [:bp (:stack-pointer state)]]
  (let [l1 (genkey) l2 (genkey)]
    [['jne l1]
     ['mov :ax (immediate-rep true)]
     ['jmp l2]
     l1
     ['mov :ax (immediate-rep false)]
     l2]))

(defprimitive < [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer (- wordsize)))
  ['xor :bx :bx]
  ['cmp :ax [:bp (:stack-pointer state)]]
  ['setb :bl]
  ['mov :ax :bx]
  ['sal :ax 7]
  ['or :ax +boolean-tag+])

(defprimitive write-char [x]
  (compile-expr x state)
  ['mov :ah 0x0e]
  ['int 0x10])

(defprimitive byte [x]
  ['mov :al x])

(defprimitive inc [x]
  (compile-expr x state)
  ['add :ax (immediate-rep 1)])

(defprimitive nil? [x]
  (compile-expr (second x) state)
  ['cmp :ax +nil+]
  ['mov :ax 0]
  ['sete :al]
  ['sal :ax 7]
  ['or :ax +boolean-tag+])

(defprimitive cons [x y]
  (compile-expr x state)
  ['mov [:si] :ax]
  (compile-expr y state)
  ['mov [:si wordsize] :ax]
  ['mov :ax :si]
  ['or :ax +cons-tag+]
  ['add :si 8])

(defprimitive car [x]
  (compile-expr x state)
  ['mov :bx :ax]
  ['mov :ax [:bx -1]])

(defprimitive recur [& exprs]
  (for [[i expr] (map-indexed vector exprs)]
    [(compile-expr expr (update state :stack-pointer (- (* i wordsize))))
     ['mov [:bp (- (:stack-pointer state) (* i wordsize))] :ax]])
  (for [i (range (count exprs))]
    [['mov :bx [:bp (- (:stack-pointer state) (* i wordsize))]]
     ['mov [:bp (- (* (inc i) wordsize))] :bx]])
  ['jmp (:recur-point state)])

(defprimitive init-graph []
  ['mov :ax 0x13]
  ['int 0x10]
  ['mov :ax 0xa000]
  ['mov :es :ax])

(defprimitive put-pixel [x y c]
  ['mov :cx :di]
  (compile-expr y state)
  ['sal :ax 4]
  ['mov :di :ax]
  ['sal :ax 2]
  ['add :di :ax]
  (compile-expr x state)
  ['sar :ax 2]
  ['add :di :ax]
  (compile-expr c state)
  ['sar :ax 2]
  ['stosb]
  ['mov :di :cx])

(defprimitive cdr [x]
  (compile-expr x state)
  ['mov :bx :ax]
  ['mov :ax [:bx (dec wordsize)]])

(defprimitive closure [label free-vars]
  ['mov [:si] (-> label name keyword)]
  ['add :si wordsize]
  (for [fvar free-vars]
    [(compile-expr fvar state)
     ['mov [:si] :ax]
     ['add :si wordsize]])
  ['mov :ax :si]
  ['sub :ax (* wordsize (inc (count free-vars)))]
  ['or :ax +closure-tag+]
  ['add :si 7]
  ['and :si 0xfff8])

(def initial-compilation-state
  {:code []
   :stack-pointer (- wordsize)
   :environment {}
   :recur-point nil})

(defn compile-var-access
  [symbol environment]
  (let [{:keys [type offset]} (environment symbol)]
    (condp = type
      :bound [['mov :ax [:bp offset]]]
      :free  [['mov :bx :di]
              ['mov :ax [:bx (- offset +closure-tag+)]]]
      :var   [['mov :ax [offset]]]
      (throw (Exception. (str "Unbound variable: " symbol))))))

(defn compile-def 
  [symbol expr {:keys [stack-pointer environment] :as state}]
  (let [address (+ 0x10000 stack-pointer)]
    {:code (codeseq
            (:code state)
            (compile-expr expr state)
            ['mov [address] :ax]
            ['sub :sp 2]
            ['sub :bp 2]),
     :environment (conj environment (make-environment-element symbol :var address)),
     :stack-pointer stack-pointer}))

(defn add-comment
  [comm res]
  (if (map? res)
    (update-in res [:code] #(vec (cons ['comment comm] %)))
    (vec (cons ['comment comm] res))))

(defn compile-expr
  ([x] (compile-expr x initial-compilation-state))
  ([x state]
     (add-comment
      x
      (cond (immediate? x)
            [['mov :ax (immediate-rep x)]]
            (variable? x)
            (compile-var-access x (:environment state))
            (primcall? x)
            (if-let [prim (primitives (first x))]
              (prim state x)
              (condp = (first x)
                'let (compile-let (second x) (next (next x)) false state)
                'loop (compile-let (second x) (next (next x)) true (assoc state :recur-point (genkey)))
                'if (compile-if (second x) (nth x 2) (nth x 3) state)
                'do (apply concat (map #(compile-expr % state) (rest x)))
                'fncall (compile-call (second x) (next (next x)) state)
                'labels (compile-labels (second x) (nth x 2) state)
                'def (compile-def (second x) (nth x 2) state)
                (throw (Exception. (format "Unknown primitive: %s" (first x))))))))))

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

   (= (first expr) 'def)
   (let [[closures new-expr] (collect-closures bound-vars (nth expr 2))]
     [closures `(~'def ~(second expr) ~new-expr)])

   :otherwise
   (let [transformed-fn (collect-closures bound-vars (first expr))
         transformed-body (map (partial collect-closures bound-vars) (next expr))]
     [(apply concat (first transformed-fn) (map first transformed-body))
      `(~'fncall ~(second transformed-fn) ~@(map second transformed-body))])))

(defn clojure->tcr
  [prog]
  (let [[closures expr] (collect-closures #{} prog)]
    (if (seq closures)
      (if (= (first expr) 'def)
        `(~'def ~(second expr) (~'labels ~(vec (apply concat closures)) ~(nth expr 2)))
        `(~'labels ~(vec (apply concat closures)) ~expr))
      expr)))

(defn compile-program*
  [sexps]
  (reduce
   (fn [{:keys [code env stack-start] :as state} sexp]
     (let [res (compile-expr (clojure->tcr sexp) state)]
       (if (map? res)
         (into state res)
         (update state :code (concat res)))))
   initial-compilation-state
   sexps))

(defn compile-program 
  [sexps]
  (concat prolog
          (:code (compile-program* sexps))
          epilog))

(defn compile-file
  [f]
  (compile-program (read-all f)))
