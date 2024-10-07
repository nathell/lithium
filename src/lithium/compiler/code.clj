(ns lithium.compiler.code
  (:require [clojure.string :as string]
            [lithium.compiler.state :as state]))

(declare compile-expr)

(defn combine [state new-instr-or-state]
  (cond (and (sequential? new-instr-or-state) (= (first new-instr-or-state) ':subexpr))
        (let [[_ expr state-modifier restore-fn] new-instr-or-state]
          ((or restore-fn state/restore-env)
           state
           (compile-expr expr (cond-> state state-modifier state-modifier))))
        (fn? new-instr-or-state) (new-instr-or-state state)
        :else (update state :code conj new-instr-or-state)))

(defn codeseq [state & code]
  (reduce combine state code))

(defn genkey []
  (-> (gensym) name string/lower-case keyword))
