(ns lithium.compiler.state
  (:require [lithium.compiler.repr :as repr]))

(def initial-compilation-state
  {:code []
   :stack-pointer (- repr/wordsize)
   :global-env-start (- 0x10000 repr/wordsize)
   :environment {}
   :recur-point nil
   :min-sp (- repr/wordsize)})

(defn next-loc [state]
  (update state :stack-pointer - repr/wordsize))

(defn restore-env [orig-state state]
  (into orig-state
        {:code (:code state)
         :min-sp (min (:min-sp orig-state) (:stack-pointer state))}))
