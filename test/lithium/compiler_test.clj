(ns lithium.compiler-test
  (:require [lithium.compiler :as compiler]
            [lithium.compiler.repr :as repr]
            [clojure.test :refer [deftest are]]))

(deftest test-compiler
  (are [expr expected-result] (let [{:keys [ax]} (compiler/compile-and-run! [expr] compiler/register-dump)]
                                (= ax (repr/immediate expected-result)))
    '(+ 3 4) 7
    '(< 10 15) true
    '(if true 3 4) 3
    '(if (< 15 10) 3 4) 4
    '(let [x 2 y (+ x 3)]
       (+ x y)) 7
    '(let [x 2
           f (fn [y]
               (+ x y))]
       (f 3)) 5))
