(ns lithium.compiler
  (:require [clojure.string :as string]
            [lithium.assembler :as assembler]
            [lithium.compiler.code :refer [codeseq compile-expr genkey]]
            [lithium.compiler.primitives :as primitives]
            [lithium.compiler.repr :as repr]
            [lithium.utils :refer [read-all]]))

(def prolog [['cli]
             ['mov :bp :sp]
             ['mov :si :heap-start]
             ['add :si 7]
             ['and :si 0xfff8]])
(def epilog [:forever ['jmp :forever] :heap-start])

(defn primcall? [x]
  (or (list? x) (seq? x)))

(defn third [x] (nth x 2))

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
            (-> state
                (update :stack-pointer - 2)
                (update :environment conj (make-environment-element v :bound stack-pointer)))])
         [[] state]
         (partition 2 bindings))]
    (concat code
            (when loop? [(:recur-point state)])
            (mapcat #(compile-expr % state) body))))

(defn variable? [x]
  (symbol? x))

(defn compile-if
  [test-expr then-expr else-expr state]
  (let [l0 (genkey) l1 (genkey)]
    (codeseq
     (compile-expr test-expr state)
     ['cmp :ax (repr/immediate false)]
     ['je l0]
     ['cmp :ax (repr/immediate nil)]
     ['je l0]
     (compile-expr then-expr state)
     ['jmp l1]
     l0
     (compile-expr else-expr state)
     l1)))

(defn compile-call
  [expr args {:keys [environment] :as state}]
  (codeseq
   ['sub :sp (* repr/wordsize (+ 2 (count environment)))]
   (map-indexed
    (fn [i expr]
      (codeseq
       (compile-expr expr (update state :stack-pointer - (* repr/wordsize (inc i))))
       ['push :ax]))
    args)
   (compile-expr expr (update state :stack-pointer - (* repr/wordsize (inc (count args)))))
   ['add :sp (* repr/wordsize (+ 2 (count args)))]
   ['push :di]
   ['mov :di :ax]
   ['mov :bx :ax]
   ['mov :bp :sp]
   ['call [:bx (- repr/closure-tag)]]
   ['pop :di]
   ['add :sp (* repr/wordsize (count environment))]
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
        ['sub :bp repr/wordsize]
        (let [arg-env (map-indexed (fn [i x]
                                     (make-environment-element x :bound (- -2 i i))) args)
              fvar-env (map-indexed (fn [i x]
                                      (make-environment-element x :free (+ 2 i i))) fvars)]
          (compile-expr body
                        (-> state
                            (update :stack-pointer - (* repr/wordsize (count args)))
                            (update :environment into (concat arg-env fvar-env)))))
        ['add :bp repr/wordsize]
        ['ret]))
     body-label
     (compile-expr code state))))

(def initial-compilation-state
  {:code []
   :stack-pointer (- repr/wordsize)
   :global-env-start (- 0x10000 repr/wordsize)
   :environment {}
   :recur-point nil})

(defn compile-var-access
  [symbol environment]
  (let [{:keys [type offset]} (environment symbol)]
    (condp = type
      :bound [['mov :ax [:bp offset]]]
      :free  [['mov :bx :di]
              ['mov :ax [:bx (- offset repr/closure-tag)]]]
      :var   [['mov :ax [offset]]]
      (throw (Exception. (str "Unbound variable: " symbol))))))

(defn compile-def
  [symbol expr {:keys [stack-pointer environment global-env-start] :as state}]
  {:code (codeseq
          (:code state)
          (compile-expr expr state)
          ['mov [global-env-start] :ax]
          ['sub :sp 2]
          ['sub :bp 2]),
   :environment (conj environment (make-environment-element symbol :var global-env-start)),
   :stack-pointer stack-pointer,
   :global-env-start (- global-env-start repr/wordsize)})

(defn add-comment
  [comm res]
  (if (map? res)
    (update-in res [:code] #(vec (cons ['comment comm] %)))
    (vec (cons ['comment comm] res))))

(defn compile-expr*
  ([x] (compile-expr* x initial-compilation-state))
  ([x state]
     (add-comment
      x
      (cond (repr/immediate? x)
            [['mov :ax (repr/immediate x)]]
            (variable? x)
            (compile-var-access x (:environment state))
            (primcall? x)
            (if-let [prim (primitives/primitives (first x))]
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

(alter-var-root #'compile-expr (constantly compile-expr*))

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

   (or (#{'if 'do 'recur} (first expr)) (primitives/primitives (first expr)))
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
         (update state :code concat res))))
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

(defn compile-and-run!
  [f]
  (assembler/run-program! (compile-file f)))
