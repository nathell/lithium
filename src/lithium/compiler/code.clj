(ns lithium.compiler.code
  (:require [clojure.string :as string]))

(defn instr? [x]
  (or (not (sequential? x)) (symbol? (first x))))

(defn codeseq [& code]
  (filter instr? (rest (tree-seq (complement instr?) seq code))))

(declare compile-expr)

(defn genkey []
  (-> (gensym) name string/lower-case keyword))
