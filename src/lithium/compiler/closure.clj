(ns lithium.compiler.closure
  (:require [lithium.compiler.ast :as ast]))

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

(defn collect-closures [ast]
  (let [codes (atom [])
        outer-xform (fn [ast]
                      (ast/match ast
                                 :fn [args body bound-vars]
                                 (let [label (gensym "cl")]
                                   (swap! codes conj {:type :code, :label label, :args args, :bound-vars bound-vars, :body body})
                                   {:type :closure, :label label, :vars bound-vars})))
        transformed (ast/walk ast identity outer-xform)]
    {:type :labels, :labels @codes, :body transformed}))
