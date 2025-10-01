(ns lithium.compiler.primitives
  (:require
   [lithium.compiler.repr :as repr]
   [lithium.compiler.state :refer [next-loc] :as state]
   [lithium.compiler.code :refer [codeseq compile-expr genkey]]))

(def primitives {})

(defmacro defprimitive [name args & code]
  `(def primitives
     (assoc primitives '~name
            (fn [~'state [~@args]]
              ~@code))))

(defprimitive + [a b]
  (codeseq
   state
   [:subexpr b]
   ['mov [:bp (:stack-pointer state)] :ax]
   [:subexpr a next-loc]
   ['add :ax [:bp (:stack-pointer state)]]))

(defprimitive - [a b]
  (codeseq
   state
   [:subexpr b]
   ['mov [:bp (:stack-pointer state)] :ax]
   [:subexpr a next-loc]
   ['sub :ax [:bp (:stack-pointer state)]]))

(defprimitive * [a b]
  (codeseq
   state
   [:subexpr b]
   ['sar :ax 2]
   ['mov [:bp (:stack-pointer state)] :ax]
   [:subexpr a next-loc]
   ['mul [:word :bp (:stack-pointer state)]]))

(defprimitive mod [a b]
  (codeseq
   state
   [:subexpr b]
   ['sar :ax 2]
   ['mov [:bp (:stack-pointer state)] :ax]
   [:subexpr a next-loc]
   ['mov :dx 0]
   ['sar :ax 2]
   ['div [:word :bp (:stack-pointer state)]]
   ['mov :ax :dx]
   ['sal :ax 2]))

(defprimitive quot [a b]
  (codeseq
   state
   [:subexpr b]
   ['sar :ax 2]
   ['mov [:bp (:stack-pointer state)] :ax]
   [:subexpr a next-loc]
   ['mov :dx 0]
   ['sar :ax 2]
   ['div [:word :bp (:stack-pointer state)]]
   ['sal :ax 2]))

(defprimitive = [a b]
  (let [l1 (genkey) l2 (genkey)]
    (codeseq
     state
     [:subexpr b]
     ['mov [:bp (:stack-pointer state)] :ax]
     [:subexpr a next-loc]
     ['cmp :ax [:bp (:stack-pointer state)]]
     ['jne l1]
     ['mov :ax (repr/immediate true)]
     ['jmp l2]
     l1
     ['mov :ax (repr/immediate false)]
     l2)))

(defprimitive < [a b]
  (codeseq
   state
   [:subexpr b]
   ['mov [:bp (:stack-pointer state)] :ax]
   [:subexpr a next-loc]
   ['xor :bx :bx]
   ['cmp :ax [:bp (:stack-pointer state)]]
   ['setb :bl]
   ['mov :ax :bx]
   ['sal :ax 7]
   ['or :ax repr/boolean-tag]))

(defprimitive write-char [x]
  (codeseq
   state
   [:subexpr x]
   ['sar :ax 8]
   ['mov :ah 0x0e]
   ['int 0x10]))

(defprimitive byte [{:keys [value]}]
  (codeseq
   state
   ['mov :al value]))

(defprimitive inc [x]
  (codeseq
   state
   [:subexpr x]
   ['add :ax (repr/immediate 1)]))

(defprimitive nil? [x]
  (codeseq
   state
   [:subexpr x]
   ['cmp :ax repr/nil-tag]
   ['mov :ax 0]
   ['sete :al]
   ['sal :ax 7]
   ['or :ax repr/boolean-tag]))

(defprimitive cons [x y]
  (codeseq
   state
   [:subexpr x]
   ['mov [:si] :ax]
   [:subexpr y]
   ['mov [:si repr/wordsize] :ax]
   ['mov :ax :si]
   ['or :ax repr/cons-tag]
   ['add :si 8]))

(defprimitive car [x]
  (codeseq
   state
   [:subexpr x]
   ['mov :bx :ax]
   ['mov :ax [:bx -1]]))

(defprimitive recur [& exprs]
  (let [orig-state state]
    (as-> state state
      (reduce (fn [state expr]
                (codeseq
                 state
                 [:subexpr expr]
                 ['mov [:bp (:stack-pointer state)] :ax]
                 next-loc))
              state
              exprs)
      (state/restore-env orig-state state)
      (reduce (fn [state [i symbol]]
                (codeseq
                 state
                 ['mov :bx [:bp (- (:stack-pointer state) (* i repr/wordsize))]]
                 ['mov [:bp (get-in state [:environment symbol :offset])] :bx]))
              state
              (map-indexed vector (:loop-symbols state)))
      (codeseq
       state
       ['jmp (:recur-point state)]))))

(defprimitive init-graph []
  (codeseq
   state
   ['mov :ax 0x13]
   ['int 0x10]
   ['mov :ax 0xa000]
   ['mov :es :ax]))

(defprimitive put-pixel [x y c]
  (codeseq
   state
   ['mov :cx :di]
   [:subexpr y]
   ['sal :ax 4]
   ['mov :di :ax]
   ['sal :ax 2]
   ['add :di :ax]
   [:subexpr x]
   ['sar :ax 2]
   ['add :di :ax]
   [:subexpr c]
   ['sar :ax 2]
   ['stosb]
   ['mov :di :cx]))

(defprimitive cdr [x]
  (codeseq
   state
   [:subexpr x]
   ['mov :bx :ax]
   ['mov :ax [:bx (dec repr/wordsize)]]))

(defprimitive int->char [x]
  (codeseq
   state
   [:subexpr x]
   ['sal :ax 6]
   ['mov :al repr/char-tag]))

(defprimitive strlen [x]
  (codeseq
   state
   [:subexpr x]
   ['mov :bx :ax]
   ['sub :bx repr/string-tag]
   ['xor :ax :ax]
   ['mov :al [:bx]]
   ['sal :ax 2]))

(defprimitive symbol->string [x]
  (codeseq
   state
   [:subexpr x]
   ['sub :ax (- repr/symbol-tag repr/string-tag)]))

(defprimitive char-at [s i]
  (codeseq
   state
   [:subexpr s]
   ['mov :bx :ax]
   ['sub :bx repr/string-tag]
   [:subexpr i]
   ['sar :ax 2]
   ['inc :ax]
   ['add :bx :ax]
   ['mov :ah [:bx]]
   ['mov :al repr/char-tag]))

(defprimitive vector-length [v]
  (codeseq
   state
   [:subexpr v]
   ['mov :bx :ax]
   ['sub :bx repr/vector-tag]
   ['mov :ax [:bx]]))

(defprimitive vector-at [v n]
  (codeseq
   state
   [:subexpr v]
   ;; the tag for vectors is 2 and we should first subtract it
   ;; from ax, then skip again by 2 omitting the length field –
   ;; this cancels out
   ['mov :bx :ax]
   [:subexpr n]
   ['sar :ax 1]
   ['add :bx :ax]
   ['mov :ax [:bx]]))

(defmacro deftypecheck [name tag-width tag]
  `(defprimitive ~name [x#]
     (let [l1# (genkey)]
       (codeseq
        ~'state
        [:subexpr x#]
        ['and :ax ~(dec (bit-shift-left 1 tag-width))]
        ['cmp :ax ~tag]
        ['mov :ax (repr/immediate false)]
        ['jne l1#]
        ['add :ax 128]
        l1#))))

(deftypecheck number? 2 repr/number-tag)
(deftypecheck vector? 3 repr/vector-tag)
(deftypecheck fn? 3 repr/closure-tag)
(deftypecheck cons? 3 repr/cons-tag)
(deftypecheck string? 3 repr/string-tag)
(deftypecheck keyword? 3 repr/symbol-tag)
(deftypecheck char? 8 repr/symbol-tag)
(deftypecheck boolean? 7 repr/boolean-tag)
