(ns lithium
  (:use [clojure.core.match :only [match]]
        [clojure.java.shell :only [sh]]))

(defmacro deftable [name headers & data]
  `(def ~name
        (into {}
              (for [~(vec headers) ~(vec (map vec (partition (count headers) data)))]
                {~(first headers) (zipmap ~(vec (map keyword (rest headers))) ~(vec (rest headers)))}))))

(deftable +registers+
  [reg size value type]
   :ax 16   0     :general
   :bx 16   3     :general
   :cx 16   1     :general
   :dx 16   2     :general
   :sp 16   4     :general
   :bp 16   5     :general
   :si 16   6     :general
   :di 16   7     :general
   :cs 16   1     :segment
   :ds 16   3     :segment
   :es 16   0     :segment
   :ss 16   2     :segment
   :al 8    0     :general
   :ah 8    4     :general
   :bl 8    3     :general
   :bh 8    7     :general
   :cl 8    1     :general
   :ch 8    5     :general
   :dl 8    2     :general
   :dh 8    6     :general)

(def +general-register-set+ (set (map key (filter #(= (:type (val %)) :general) +registers+))))
(def +segment-register-set+ (set (map key (filter #(= (:type (val %)) :segment) +registers+))))

(defn operand-type [operand]
  (cond
   (+general-register-set+ operand) :reg
   (+segment-register-set+ operand) :sreg
   (integer? operand)       :imm
   (keyword? operand)       :label))

(defn imm [n v]
  (let [maxv (dec (bit-shift-left 1 n))]
    (when-not (<= 0 v maxv)
      (throw (Exception. (format "Incorrect %d-bit immediate value: %s" n v))))
    [n v]))

(defn modrm
  [mod spare rm]
  (+ (bit-shift-left mod 6)
     (bit-shift-left spare 3)
     rm))

(defn decompose-instruction [[instr op1 op2]]
  (match [(-> instr name keyword) (operand-type op1) (operand-type op2)]
    [:mov :reg :imm] {:opcode (+ (if (= (-> op1 +registers+ :size) 8) 0xb0 0xb8)
                                 (-> op1 +registers+ :value)),
                      :immediate (imm (-> op1 +registers+ :size) op2)}
    [:mov :sreg :reg] {:opcode 0x8e,
                       :modrm (modrm 3 (-> op1 +registers+ :value) (-> op2 +registers+ :value))}
    [:xor :reg :reg] {:opcode (if (= (-> op1 +registers+ :size) 8) 0x30 0x31),
                      :modrm (modrm 3 (-> op1 +registers+ :value) (-> op2 +registers+ :value))}
    [:push :reg nil] {:opcode (+ 0x50 (-> op1 +registers+ :value))}
    [:pop :reg nil] {:opcode (+ 0x58 (-> op1 +registers+ :value))}
    [:stosb nil nil] {:opcode 0xaa}
    [:ret nil nil] {:opcode 0xc3}
    [:inc :reg nil] (if (= (-> op1 +registers+ :size) 8)
                      {:opcode 0xfe,
                       :modrm (modrm 3 0 (-> op1 +registers+ :value))}
                      {:opcode (+ 0x40 (-> op1 +registers+ :value))})
    [:cmp :reg :imm] {:opcode (cond (= op1 :al) 0x3c
                                    (= op1 :ax) 0x3d
                                    (= (-> op1 +registers+ :size) 8) 0x80
                                    true 0x81),
                      :modrm (when-not (#{:al :ax} op1)
                               (modrm 3 7 (-> op1 +registers+ :value)))
                      :immediate (imm (-> op1 +registers+ :size) op2)}
    [:jne :label nil] {:opcode 0x75, :immediate [8 op1]}
    [:loop :label nil] {:opcode 0xe2, :immediate [8 op1]}
    [:int :imm nil]  (if (= op1 3)
                       {:opcode 0xcc}
                       {:opcode 0xcd, :immediate (imm 8 op1)})))

(defn word-to-bytes [[size w]]
  (condp = size
      8 [w]
      16 [(bit-and w 0xff) (bit-shift-right w 8)]))

(defn instruction->bytes
  [{:keys [prefixes opcode modrm sib displacement immediate]}]
  (vec (concat
        [opcode]
        (when modrm [modrm])
        (when immediate (word-to-bytes immediate)))))

(def assemble-instruction (comp instruction->bytes decompose-instruction))

(defn unsigned-byte [x]
  (if (< x 0) (+ x 256) x))

(defn resolve-labels [code labels]
  (loop [result [] code code pos 0]
    (if-let [fb (first code)]
      (recur (conj result (if (keyword? fb)
                            (unsigned-byte (dec (- (labels fb) pos)))
                            fb))
             (next code) (inc pos))
      result)))

(defn asm [prog]
  (loop [prog prog code [] pc 0 labels {}]
    (if-not (seq prog)
      (resolve-labels code labels)
      (let [ins (first prog)]
        (if (keyword? ins)
          (recur (next prog) code pc (assoc labels ins pc))
          (let [assembled (assemble-instruction ins)
                cnt (count assembled)]
            (recur (next prog) (into code assembled) (+ pc cnt) labels)))))))

(defn run! [prog]
  (let [filename "/tmp/a.com"
        assembled (asm (if (string? prog) (read-string (str "[" (slurp prog) "]")) prog))]
    (with-open [f (java.io.FileOutputStream. filename)]
      (.write f (into-array Byte/TYPE (map #(byte (if (>= % 128) (- % 256) %)) assembled)))
      (sh "dosbox" filename)
      nil)))