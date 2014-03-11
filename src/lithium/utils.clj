(ns lithium.utils
  (:require [clojure.java.io :as io])
  (:use [clojure.test :only [deftest is]]))

(defmacro update
  "update is to update-in what assoc is to assoc-in."
  ([m k v]
     `(let [m# ~m oldv# (m# ~k)]
        (assoc m# ~k (~(if (list? v) (first v) v)
                      oldv#
                      ~@(if (list? v) (rest v))))))
  ([m k v & kvs]
     `(update (update ~m ~k ~v) ~@kvs)))

(deftest test-update
  (is (= (update {:a 10 :b 10 :c 10} :a (- 2) :b dec)
         {:a 8 :b 9 :c 10})))

(defn read-all
  [input]
  (with-open [f (java.io.PushbackReader. (io/reader input))]
    (let [eof (Object.)]
      (doall (take-while #(not= % eof) (repeatedly #(read f false eof)))))))

