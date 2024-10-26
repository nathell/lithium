(ns lithium.compiler.closure
  (:require [lithium.compiler.ast :as ast]
            [lithium.compiler.code :refer [genkey]]))

(defn free-variable-analysis [ast]
  (ast/walk
   (assoc ast :bound-vars [])
   (fn [ast]
     (ast/match ast
                ast/let-like [bound-vars bindings body]
                (let [bindings (loop [acc []
                                      bound-vars bound-vars
                                      bindings bindings]
                                 (let [{:keys [symbol expr] :as binding} (first bindings)]
                                   (if binding
                                     (recur (conj acc {:symbol symbol, :expr (assoc expr :bound-vars bound-vars)})
                                            (conj bound-vars symbol)
                                            (next bindings))
                                     acc)))
                      body-bound-vars (into bound-vars (map :symbol bindings))]
                  (assoc ast :bindings bindings :body (map #(assoc % :bound-vars body-bound-vars) body)))

                :fn [args body bound-vars]
                (let [body-bound-vars (into bound-vars args)]
                  (assoc ast :body (map #(assoc % :bound-vars body-bound-vars) body)))

                ;; otherwise
                (ast/expr-update ast assoc :bound-vars (:bound-vars ast))))
   identity))

(defn long-value?
  "Returns true if ast represents a 'long value', i.e. one whose content can't fit into a register."
  [ast]
  (and (= (:type ast) :value)
       (= (:value-type ast) :string)))

(defn collect-closures [ast]
  (let [codes (atom [])
        long-values (atom {})
        outer-xform (fn [ast]
                      (ast/match ast
                        long-value? [value value-type]
                        #_=> (let [ref (get (swap! long-values update value #(or % {:value-type value-type, :ref (genkey)})) value)]
                               (merge {:type :long-value-ref} ref))
                        :fn [args body bound-vars]
                        #_=> (let [label (gensym "cl")]
                               (swap! codes conj {:type :code, :label label, :args args, :bound-vars bound-vars, :body body})
                               {:type :closure, :label label, :vars bound-vars})))
        transformed (ast/walk ast identity outer-xform)]
    (if (or (seq @codes) (seq @long-values))
      {:type :labels, :labels @codes, :long-values @long-values, :body transformed}
      transformed)))
