(ns lithium.compiler
  (:require [clojure.string :as string]
            [lithium.assembler :as assembler]
            [lithium.compiler.ast :as ast]
            [lithium.compiler.closure :as closure]
            [lithium.compiler.code :refer [codeseq compile-expr genkey]]
            [lithium.compiler.primitives :as primitives]
            [lithium.compiler.repr :as repr]
            [lithium.compiler.state :as state]
            [lithium.driver :as driver]
            [lithium.utils :refer [read-all]]))

(def prolog
  [['cli]
   ['mov :bp :sp]
   ['mov :si :heap-start]
   ['add :si 7]
   ['and :si 0xfff8]
   [:reserve-tmp-space]])  ;; reserve some space between bp and sp for temporary values

(def endless-loop-epilog
  [:forever
   ['jmp :forever]])

(def dos-exit-epilog
  [['mov :ax 0x4c00]
   ['int 0x21]])

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
  [bindings body loop? orig-state]
  (as-> orig-state state
    (reduce
     (fn [{:keys [stack-pointer] :as state} {:keys [symbol expr]}]
       (-> state
           (codeseq
            [:subexpr expr]
            ['mov [:bp stack-pointer] :ax]
            state/next-loc)
           (update :environment conj (make-environment-element symbol :bound stack-pointer))))
     state
     bindings)
    (if loop?
      (assoc (codeseq state (:recur-point state)) :loop-symbols (mapv :symbol bindings))
      state)
    (reduce #(codeseq %1 [:subexpr %2]) state body)
    (state/restore-env orig-state state)))

(defn compile-if
  [test-expr then-expr else-expr state]
  (let [l0 (genkey) l1 (genkey)]
    (codeseq
     state
     [:subexpr test-expr]
     ['cmp :ax (repr/immediate false)]
     ['je l0]
     ['cmp :ax (repr/immediate nil)]
     ['je l0]
     [:subexpr then-expr]
     ['jmp l1]
     l0
     [:subexpr else-expr]
     l1)))

(defn compile-call
  [expr args orig-state]
  (as-> orig-state state
    (codeseq
     state
     [:subexpr expr]
     ['push :di]
     ['mov :di :ax])
    (reduce
     #(codeseq
       (compile-expr %2 %1)
       ['push :ax])
     state
     (reverse args))
    (codeseq
     state
     ['mov :bx :di]
     ['call [:bx (- repr/closure-tag)]]
     ['add :sp (* repr/wordsize (count args))]
     ['pop :di])
    (state/restore-env orig-state state)))

(defn compile-long-value
  [state [val {:keys [ref value-type]}]]
  (as-> state state
    (codeseq state ['align 8] ref)
    (case value-type
      :string (codeseq state ['bytes [(count val)]] ['string val]))))

(defn compile-labels
  [labels long-values code orig-state]
  (let [body-label (genkey)]
    (as-> orig-state state
      (codeseq
       state
       ['jmp body-label])
      (reduce compile-long-value
              state
              long-values)
      (reduce (fn [state {:keys [label args bound-vars body]}]
                (codeseq
                 state
                 (keyword (name label))
                 ['push :bp]
                 ['mov :bp :sp]
                 [:reserve-tmp-space]
                 (let [arg-env  (map-indexed (fn [i x]
                                               (make-environment-element x :bound (* repr/wordsize (+ i 2)))) args)
                       fvar-env (map-indexed (fn [i x]
                                               (make-environment-element x :free (* repr/wordsize (inc i)))) bound-vars)]
                   [:subexpr
                    (first body)      ; FIXME needs multi-expr support
                    #(-> %
                         (assoc :stack-pointer (- repr/wordsize)
                                :min-sp (- repr/wordsize))
                         (update :environment into (concat arg-env fvar-env)))])
                 ['mov :sp :bp]
                 ['pop :bp]
                 #(update % :code conj [:set-tmp-space (- (- repr/wordsize) (:min-sp %))])
                 ['ret]))
              state
              labels)
      (assoc state :min-sp (:min-sp orig-state))
      (codeseq
       state
       body-label
       [:subexpr code
        identity
        (fn [orig-state state]
          (assoc (state/restore-env orig-state state)
                 :environment (:environment state)
                 :global-env-start (:global-env-start state)))]))))

(defn compile-var-access
  [symbol {:keys [environment] :as state}]
  (let [{:keys [type offset]} (environment symbol)]
    (condp = type
      :bound (codeseq state ['mov :ax [:bp offset]])
      :free  (codeseq state
                      ['mov :bx :di]
                      ['mov :ax [:bx (- offset repr/closure-tag)]])
      :var   (codeseq state ['mov :ax [offset]])
      (throw (Exception. (str "Unbound variable: " symbol))))))

(defn compile-def
  [symbol expr {:keys [stack-pointer global-env-start] :as state}]
  (-> (codeseq
       state
       [:subexpr expr]
       ['mov [global-env-start] :ax]
       ['sub :sp 2]
       ['sub :bp 2])
      (update :environment conj (make-environment-element symbol :var global-env-start))
      (update :global-env-start - repr/wordsize)))

(defn compile-closure
  [label free-vars orig-state]
  (as-> orig-state state
    (codeseq state
     ['mov [:si] (-> label name keyword)]
     ['add :si repr/wordsize])
    (reduce (fn [state fvar]
              (codeseq
               state
               [:subexpr {:type :env-lookup, :symbol fvar}]
               ['mov [:si] :ax]
               ['add :si repr/wordsize]))
            state free-vars)
    (codeseq
     state
     ['mov :ax :si]
     ['sub :ax (* repr/wordsize (inc (count free-vars)))]
     ['or :ax repr/closure-tag]
     ['add :si 7]
     ['and :si 0xfff8])))

(defn compile-value [{:keys [value]} state]
  (codeseq state
           ['mov :ax (repr/immediate value)]))

(defn compile-long-value-ref [ref value-type state]
  (codeseq state ['mov :ax [:+ ref (repr/type->tag value-type)]]))

(defn compile-expr*
  [ast state]
  (let [state (codeseq state ['comment (ast/ast->clojure ast)])]
    (ast/match ast
      :value []                           (compile-value ast state)
      :env-lookup [symbol]                (compile-var-access symbol state)
      :primitive-call [primitive args]    ((primitives/primitives primitive) state args)
      :def [symbol definition]            (compile-def symbol definition state)
      :let [bindings body]                (compile-let bindings body false state)
      :loop [bindings body]               (compile-let bindings body true (assoc state :recur-point (genkey)))
      :if [condition then-expr else-expr] (compile-if condition then-expr else-expr state)
      :do [exprs]                         (reduce #(compile-expr %2 %1) state exprs)
      :fn-call [fn-expr args]             (compile-call fn-expr args state)
      :labels [labels long-values body]   (compile-labels labels long-values body state)
      :closure [label vars]               (compile-closure label vars state)
      :long-value-ref [ref value-type]    (compile-long-value-ref ref value-type state)
      (throw (ex-info "Unable to compile" ast)))))

(alter-var-root #'compile-expr (constantly compile-expr*))

(defn prepare-sexp [sexp]
  (-> sexp
      ast/clojure->ast
      closure/free-variable-analysis
      closure/collect-closures))

(defn compile-sexp
  [state sexp]
  (-> sexp
      prepare-sexp
      (compile-expr state)))

(defn compile-program*
  [sexps]
  (reduce
   compile-sexp
   state/initial-compilation-state
   sexps))

(defn add-tmp-spaces
  [asm]
  (let [spaces (map second (filter #(and (sequential? %) (= (first %) :set-tmp-space)) asm))
        spaces (into [(last spaces)] (drop-last spaces))]
    (first
     (reduce (fn [[code spaces] instr]
               (cond
                 (and (sequential? instr) (= (first instr) :set-tmp-space)) [code spaces]
                 (= instr [:reserve-tmp-space]) [(conj code ['sub :sp (first spaces)]) (next spaces)]
                 :else [(conj code instr) spaces]))
             [[] spaces]
             asm))))

(defn compile-program
  [prog & [{:keys [epilog]
            :or {epilog endless-loop-epilog}}]]
  (let [state (compile-program* (read-all prog))
        asm (concat prolog
                    (:code state)
                    [[:set-tmp-space (- (- repr/wordsize) (:min-sp state))]]
                    epilog
                    [:heap-start])]
    (add-tmp-spaces asm)))

(defn compile-and-run!
  ([f] (compile-and-run! f dos-exit-epilog))
  ([f epilog]
   (-> f
       (compile-program {:epilog epilog})
       assembler/assemble
       (driver/run-program! (not= epilog register-dump)))))
