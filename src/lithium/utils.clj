(ns lithium.utils
  (:require [clojure.java.io :as io])
  (:use [clojure.test :only [deftest is]]))

(defn read-all
  [input]
  (with-open [f (java.io.PushbackReader. (io/reader input))]
    (let [eof (Object.)]
      (doall (take-while #(not= % eof) (repeatedly #(read f false eof)))))))
