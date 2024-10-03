(ns lithium.compiler.primitives
  (:require
   [lithium.compiler.repr :as repr]
   [lithium.compiler.code :refer [codeseq compile-expr genkey]]))

(def primitives {})

(defmacro defprimitive [name args & code]
  `(def primitives
     (assoc primitives '~name
            (fn [~'state [~@args]]
              (codeseq ~@code)))))

(defprimitive + [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer - repr/wordsize))
  ['add :ax [:bp (:stack-pointer state)]])

(defprimitive - [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer - repr/wordsize))
  ['sub :ax [:bp (:stack-pointer state)]])

(defprimitive * [a b]
  (compile-expr b state)
  ['sar :ax 2]
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer - repr/wordsize))
  ['mul [:word :bp (:stack-pointer state)]])

(defprimitive mod [a b]
  (compile-expr b state)
  ['sar :ax 2]
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer - repr/wordsize))
  ['mov :dx 0]
  ['sar :ax 2]
  ['div [:word :bp (:stack-pointer state)]]
  ['mov :ax :dx]
  ['sal :ax 2])

(defprimitive = [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer - repr/wordsize))
  ['cmp :ax [:bp (:stack-pointer state)]]
  (let [l1 (genkey) l2 (genkey)]
    [['jne l1]
     ['mov :ax (repr/immediate true)]
     ['jmp l2]
     l1
     ['mov :ax (repr/immediate false)]
     l2]))

(defprimitive < [a b]
  (compile-expr b state)
  ['mov [:bp (:stack-pointer state)] :ax]
  (compile-expr a (update state :stack-pointer - repr/wordsize))
  ['xor :bx :bx]
  ['cmp :ax [:bp (:stack-pointer state)]]
  ['setb :bl]
  ['mov :ax :bx]
  ['sal :ax 7]
  ['or :ax repr/boolean-tag])

(defprimitive write-char [x]
  (compile-expr x state)
  ['sar :ax 2]
  ['mov :ah 0x0e]
  ['int 0x10])

(defprimitive byte [{:keys [value]}]
  ['mov :al value])

(defprimitive inc [x]
  (compile-expr x state)
  ['add :ax (repr/immediate 1)])

(defprimitive nil? [x]
  (compile-expr (second x) state)
  ['cmp :ax repr/nil-tag]
  ['mov :ax 0]
  ['sete :al]
  ['sal :ax 7]
  ['or :ax repr/boolean-tag])

(defprimitive cons [x y]
  (compile-expr x state)
  ['mov [:si] :ax]
  (compile-expr y state)
  ['mov [:si repr/wordsize] :ax]
  ['mov :ax :si]
  ['or :ax repr/cons-tag]
  ['add :si 8])

(defprimitive car [x]
  (compile-expr x state)
  ['mov :bx :ax]
  ['mov :ax [:bx -1]])

(defprimitive recur [& exprs]
  (for [[i expr] (map-indexed vector exprs)]
    [(compile-expr expr (update state :stack-pointer - (* i repr/wordsize)))
     ['mov [:bp (- (:stack-pointer state) (* i repr/wordsize))] :ax]])
  (for [i (range (count exprs))]
    [['mov :bx [:bp (- (:stack-pointer state) (* i repr/wordsize))]]
     ['mov [:bp (- (* (inc i) repr/wordsize))] :bx]])
  ['jmp (:recur-point state)])

(defprimitive init-graph []
  ['mov :ax 0x13]
  ['int 0x10]
  ['mov :ax 0xa000]
  ['mov :es :ax])

(defprimitive put-pixel [x y c]
  ['mov :cx :di]
  (compile-expr y state)
  ['sal :ax 4]
  ['mov :di :ax]
  ['sal :ax 2]
  ['add :di :ax]
  (compile-expr x state)
  ['sar :ax 2]
  ['add :di :ax]
  (compile-expr c state)
  ['sar :ax 2]
  ['stosb]
  ['mov :di :cx])

(defprimitive cdr [x]
  (compile-expr x state)
  ['mov :bx :ax]
  ['mov :ax [:bx (dec repr/wordsize)]])
