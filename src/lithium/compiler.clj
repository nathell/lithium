(ns lithium.compiler
  (:require [clojure.string :as string]
            [lithium.assembler :as assembler]
            [lithium.compiler.ast :as ast]
            [lithium.compiler.closure :as closure]
            [lithium.compiler.code :refer [codeseq compile-expr genkey]]
            [lithium.compiler.primitives :as primitives]
            [lithium.compiler.repr :as repr]
            [lithium.driver :as driver]
            [lithium.utils :refer [read-all]]))

(def prolog
  [['cli]
   ['mov :bp :sp]
   ['mov :si :heap-start]
   ['add :si 7]
   ['and :si 0xfff8]
   ['sub :sp 16]]) ;; reserve some space between bp and sp for temporary values

(def endless-loop-epilog
  [:forever
   ['jmp :forever]])

(def register-dump
  ;; taken from http://www.fysnet.net/yourhelp.htm
  [['pushf]
   ['pusha]
   ['push :cs]
   ['mov :di :buff]
   ['mov :si :msg1]
   ['mov :cx 10]
   :loop1
   ['movsw]
   ['mov :al (int \=)]
   ['stosb]
   ['pop :ax]
   ['mov :bx 4]
   :ploop
   ['rol :ax 4]
   ['push :ax]
   ['and :al 0x0f]
   ['daa]
   ['add :al 0xf0]
   ['adc :al 0x40]
   ['stosb]
   ['pop :ax]
   ['dec :bx]
   ['jnz :ploop]
   ['mov :ax 0x0d0a]
   ['stosw]
   ['loop :loop1]
   ['mov :al 0x24]
   ['stosb]
   ['mov :dx :buff]
   ['mov :ah 9]
   ['int 0x21]
   ['mov :ah 0x4c]
   ['int 0x21]
   :msg1
   ['string "CSDISIBPSPBXDXCXAXFL"]
   :buff
   ['bytes (repeat 100 0)]])

(defn make-environment-element
  [symbol type offset]
  [symbol {:type type, :offset offset}])

(defn compile-let
  [bindings body loop? state]
  (let [[code state]
        (reduce
         (fn [[code {:keys [stack-pointer environment] :as state}] {:keys [symbol expr]}]
           [(codeseq
             code
             (compile-expr expr state)
             ['mov [:bp stack-pointer] :ax])
            (-> state
                (update :stack-pointer - 2)
                (update :environment conj (make-environment-element symbol :bound stack-pointer)))])
         [[] state]
         bindings)]
    (concat code
            (when loop? [(:recur-point state)])
            (mapcat #(compile-expr % (cond-> state loop? (assoc :loop-symbols (mapv :symbol bindings)))) body))))

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
   (compile-expr expr state)
   ['push :di]
   ['mov :di :ax]
   (map-indexed
    (fn [i expr]
      (codeseq
       (compile-expr expr (update state :stack-pointer - (* repr/wordsize (inc i))))
       ['push :ax]))
    (reverse args))
   ['mov :bx :di]
   ['call [:bx (- repr/closure-tag)]]
   ['add :sp (* repr/wordsize (count args))]
   ['pop :di]))

(defn compile-labels
  [labels code state]
  (let [body-label (genkey)
        body-res   (compile-expr code state)
        code       (codeseq
                    ['jmp body-label]
                    (for [{:keys [label args bound-vars body]} labels]
                      (codeseq
                       (keyword (name label))
                       ['push :bp]
                       ['mov :bp :sp]
                       ['sub :sp 16]
                       (let [arg-env  (map-indexed (fn [i x]
                                                     (make-environment-element x :bound (* repr/wordsize (+ i 2)))) args)
                             fvar-env (map-indexed (fn [i x]
                                                     (make-environment-element x :free (* repr/wordsize (inc i)))) bound-vars)]
                         (compile-expr (first body)  ; FIXME needs multi-expr support
                                       (-> state
                                           (update :stack-pointer - (* repr/wordsize (count args)))
                                           (update :environment into (concat arg-env fvar-env)))))
                       ['mov :sp :bp]
                       ['pop :bp]
                       ['ret]))
                    body-label
                    (if (map? body-res) (:code body-res) body-res))]
    (if (map? body-res)
      (assoc body-res :code code)
      code)))

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

(defn compile-closure
  [label free-vars state]
  (codeseq
   ['mov [:si] (-> label name keyword)]
   ['add :si repr/wordsize]
   (for [fvar free-vars]
     [(compile-expr {:type :env-lookup, :symbol fvar} state)
      ['mov [:si] :ax]
      ['add :si repr/wordsize]])
   ['mov :ax :si]
   ['sub :ax (* repr/wordsize (inc (count free-vars)))]
   ['or :ax repr/closure-tag]
   ['add :si 7]
   ['and :si 0xfff8]))

(defn add-comment
  [comm res]
  (if (map? res)
    (update-in res [:code] #(vec (cons ['comment comm] %)))
    (vec (cons ['comment comm] res))))

(defn compile-value [{:keys [value]}]
  [['mov :ax (repr/immediate value)]])

(defn compile-expr*
  [ast state]
  (ast/match ast
             :value []                           (compile-value ast)
             :env-lookup [symbol]                (compile-var-access symbol (:environment state))
             :primitive-call [primitive args]    ((primitives/primitives primitive) state args)
             :def [symbol definition]            (compile-def symbol definition state)
             :let [bindings body]                (compile-let bindings body false state)
             :loop [bindings body]               (compile-let bindings body true (assoc state :recur-point (genkey)))
             :if [condition then-expr else-expr] (compile-if condition then-expr else-expr state)
             :do [exprs]                         (apply concat (map #(compile-expr % state) exprs))
             :fn-call [fn-expr args]             (compile-call fn-expr args state)
             :labels [labels body]               (compile-labels labels body state)
             :closure [label vars]               (compile-closure label vars state)
             (throw (ex-info "Unable to compile" ast))))

(alter-var-root #'compile-expr (constantly compile-expr*))

(defn compile-program*
  [sexps]
  (reduce
   (fn [{:keys [code env stack-start] :as state} sexp]
     (let [res (-> sexp
                   ast/clojure->ast
                   closure/free-variable-analysis
                   closure/collect-closures
                   (compile-expr state))]
       (if (map? res)
         (into state res)
         (update state :code concat res))))
   initial-compilation-state
   sexps))

(defn compile-program
  [prog & [{:keys [epilog]
            :or {epilog endless-loop-epilog}}]]
  (concat prolog
          (:code (compile-program* (read-all prog)))
          epilog
          [:heap-start]))

(defn compile-and-run!
  ([f] (compile-and-run! f true))
  ([f wait?]
   (-> f
       (compile-program {:epilog (if wait? endless-loop-epilog register-dump)})
       assembler/assemble
       (driver/run-program! wait?))))
