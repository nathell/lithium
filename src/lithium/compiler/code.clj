(ns lithium.compiler.code
  (:require [clojure.string :as string]
            [lithium.compiler.state :as state]))

(declare compile-expr)

(defn combine [state instr]
  (cond (and (sequential? instr) (= (first instr) ':subexpr))
        #_=> (let [[_ expr state-modifier restore-fn] instr
                   orig-state state
                   state (cond-> state state-modifier state-modifier)
                   state (compile-expr expr state)]
               ((or restore-fn state/restore-env) orig-state state))
        (fn? instr)
        #_=> (instr state)
        :else (update state :code conj instr)))

(defn codeseq [state & code]
  (reduce combine state code))

(defn genkey []
  (-> (gensym) name string/lower-case keyword))
