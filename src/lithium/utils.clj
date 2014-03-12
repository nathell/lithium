(ns lithium.utils
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

