(ns lithium.assembler
  (:require [clojure.string :as string])
  (:use [clojure.java.shell :only [sh]]))

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

(def +condition-codes+
     {:o 0 :no 1 :b 2 :c 2 :nae 2 :ae 3 :nb 3 :nc 3 :e 4 :z 4 :ne 5 :nz 5 :be 6 :na 6 :a 7 :nbe 7
      :s 8 :ns 9 :p 10 :pe 10 :np 11 :po 11 :l 12 :nge 12 :ge 13 :nl 13 :le 14 :ng 14 :g 15 :nle 15})

(defn modrm
  [mod spare rm]
  (+ (bit-shift-left mod 6)
     (bit-shift-left spare 3)
     rm))

(defn reg8  [x] (let [info (+registers+ x)] (and info (= (:type info) :general) (= (:size info) 8))))
(defn reg16 [x] (let [info (+registers+ x)] (and info (= (:type info) :general) (= (:size info) 16))))
(defn sreg  [x] (let [info (+registers+ x)] (and info (= (:type info) :segment))))
(defn imm8  [x] (and (integer? x) (<= 0 x 255)))
(defn imm16 [x] (or (and (integer? x) (<= 0 x 65535)) (keyword? x)))
(defn mem   [x] (vector? x))
(defn rm8   [x] (or (reg8 x) (mem x)))
(defn rm16  [x] (or (reg16 x) (mem x)))
(defn label [x] (keyword? x))

(def assembly-table
     [[:mov rm8 reg8]     [0x88 :r]
      [:mov rm16 reg16]   [0x89 :r]
      [:mov reg8 rm8]     [0x8a :r]
      [:mov reg16 rm16]   [0x8b :r]
      [:mov reg8 imm8]    [[:r+ 0xb0] :ib]
      [:mov reg16 imm16]  [[:r+ 0xb8] :iw]
      [:mov sreg rm16]    [0x8e :r]
      [:xor rm8 reg8]     [0x30 :r]
      [:xor rm16 reg16]   [0x31 :r]
      [:push reg16]       [[:r+ 0x50]]
      [:pop reg16]        [[:r+ 0x58]]
      [:stosb]            [0xaa]
      [:ret]              [0xc3]
      [:inc reg16]        [[:r+ 0x40]]
      [:inc rm8]          [0xfe :0]
      [:cmp :al imm8]     [0x3c :ib]
      [:cmp :ax imm16]    [0x3d :iw]
      [:cmp rm8 imm8]     [0x80 :7 :ib]
      [:cmp rm16 imm16]   [0x81 :7 :iw]
      [:cmp reg8 rm8]     [0x3a :r]
      [:cmp reg16 rm16]   [0x3b :r]
      [:add rm8 reg8]     [0x00 :r]
      [:add rm16 reg16]   [0x01 :r]
      [:add reg8 rm8]     [0x02 :r]
      [:add reg16 rm16]   [0x03 :r]
      [:add :al imm8]     [0x04 :ib]
      [:add :ax imm16]    [0x05 :iw]
      [:add rm8 imm8]     [0x80 :0 :ib]
      [:add rm16 imm16]   [0x81 :0 :iw]
      [:sub rm8 imm8]     [0x80 :5 :ib]
      [:sub rm16 imm8]    [0x83 :5 :ib]
      [:sub rm16 imm16]   [0x81 :5 :iw]
      [:and rm8 imm8]     [0x80 :4 :ib]
      [:and rm16 imm8]    [0x83 :4 :ib]
      [:and rm16 imm16]   [0x81 :4 :iw]
      [:mul rm8]          [0xf6 :4]
      [:mul rm16]         [0xf7 :4]
      [:sal rm8 1]        [0xd0 :4]
      [:sal rm8 imm8]     [0xc0 :4 :ib]
      [:sal rm16 1]       [0xd1 :4]
      [:sal rm16 imm8]    [0xc1 :4 :ib]
      [:sar rm8 1]        [0xd0 :7]
      [:sar rm8 imm8]     [0xc0 :7 :ib]
      [:sar rm16 1]       [0xd1 :7]
      [:sar rm16 imm8]    [0xc1 :7 :ib]
      [:or rm8 imm8]      [0x80 :1 :ib]
      [:or rm16 imm16]    [0x81 :1 :iw]
      [:jCC label]        [[:cc+ 0x70] :rb]
      [:setCC rm8]        [0x0f [:cc+ 0x90] :2]
      [:loop label]       [0xe2 :rb]
      [:jmp label]        [0xe9 :rw]
      [:call label]       [0xe8 :rw]
      [:int 3]            [0xcc]
      [:int imm8]         [0xcd :ib]])

(defn extract-cc [instr template]
  (let [re (re-pattern (string/replace (name template) "CC" "(.+)"))]
    (when-let [cc-s (second (re-find re (name instr)))]
      (keyword cc-s))))

(defn part-of-spec-matches? [datum template]
  (if (fn? template) (template datum) (= datum template)))

(defn instruction-matches? [instr [template _]]
  (let [f1 (first instr)
        f2 (first template)]
    (and (or (= (name f1) (name f2))
             ((set (keys +condition-codes+)) (extract-cc f1 f2))) 
        (= (count instr) (count template))
         (reduce #(and %1 %2) true (map part-of-spec-matches? (rest instr) (rest template))))))

(defn find-template [instr]
  (first (filter (partial instruction-matches? instr)
                 (partition 2 assembly-table))))

(defn make-label [label width]
  (keyword (or (namespace label) (name width)) (name label)))

(defn label?
  ([x] (keyword? x))
  ([x type] (and (keyword? x) (= (namespace x) (name type)))))

(defn word-to-bytes [[size w]]
  (let [w (if (and (integer? w) (neg? w)) (+ w (bit-shift-left 1 size)) w)]
    (condp = size
        0 []
        8 [w]
        16 (if (keyword? w) [:placeholder (make-label w :abs)] [(bit-and w 0xff) (bit-shift-right w 8)]))))

(defn lenient-parse-int [x]
  (try
    (Integer/parseInt x)
    (catch NumberFormatException _ nil)))

(defn make-modrm [rm-desc spare]
  (if (keyword? rm-desc)
    [(modrm 3 spare (-> rm-desc +registers+ :value))]
    (let [registers (vec (sort-by name (filter keyword? rm-desc)))
          displacement (reduce + 0 (filter integer? rm-desc))
          rm-map {[:bx :si] 0 [:bx :di] 1 [:bp :si] 2 [:bp :di] 3 [:si] 4 [:di] 5 [:bp] 6 [] 6 [:bx] 7}
          mod (cond
               (or (and (zero? displacement) (not= registers [:bp])) (empty? registers)) 0
               (or (and (zero? displacement) (= registers [:bp])) (<= -128 displacement 127)) 1
               (<= -32768 displacement 32767) 2)
          rm (rm-map registers)]
      (when-not rm
        (throw (Exception. (format "Incorrect memory reference: %s" rm-desc))))
      (into [(modrm mod spare rm)] (word-to-bytes [(* 8 (if (empty? registers) 2 mod)) displacement])))))

(defn parse-byte [[instr op1 op2] [instr-template op1-template op2-template] byte-desc]
  (let [imm (cond (#{imm8 imm16} op1-template) op1 (#{imm8 imm16} op2-template) op2)
        rm (cond (#{rm8 rm16} op1-template) op1 (#{rm8 rm16} op2-template) op2)
        not-rm (if (= rm op1) op2 op1)]
    (cond
     (integer? byte-desc) [byte-desc]
     (= byte-desc :ib) (word-to-bytes [8 imm])
     (= byte-desc :iw) (word-to-bytes [16 imm])
     (= byte-desc :rb) [(make-label op1 :byte)]
     (= byte-desc :rw) [:placeholder (make-label op1 :word)]     
     (and (keyword? byte-desc) (lenient-parse-int (name byte-desc)))
       (make-modrm rm (lenient-parse-int (name byte-desc)))
     (= byte-desc :r)
       (make-modrm rm (-> not-rm +registers+ :value))
     (and (sequential? byte-desc) (= (first byte-desc) :r+))
       [(+ (second byte-desc) (-> op1 +registers+ :value))]
     (and (sequential? byte-desc) (= (first byte-desc) :cc+))
       [(+ (second byte-desc) (-> instr (extract-cc instr-template) +condition-codes+))])))

(defn assemble-instruction [instr]
  (let [[template parts] (find-template instr)]
    (when-not template (throw (Exception. (str "Could not assemble instruction: " (pr-str instr)))))
    (let [assembled-parts (map (partial parse-byte instr template) parts)]
      (apply concat assembled-parts))))

;; This is the value added to absolute addresses of labels, telling
;; the assembler where the code starts from. Defaults to 0x100 for
;; compatibility with COM format.
(def *origin* 0x100)

(defn resolve-labels [code labels]
  (loop [result [] code code pos 0]
    (if-let [fb (first code)]
      (recur
       (cond (= fb :placeholder) result
             (label? fb :byte)   (into result (word-to-bytes [8 (dec (- (-> fb name keyword labels) pos))]))
             (label? fb :word)   (into result (word-to-bytes [16 (dec (- (-> fb name keyword labels) pos))]))
             (label? fb :abs)    (into result (word-to-bytes [16 (+ *origin* (-> fb name keyword labels))]))
             :otherwise          (conj result fb))
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

(defn hexdump [prog]
  (string/join " " (map #(format "%02x" %) (asm prog))))

(defn assemble-file [prog out]
  (let [assembled (asm (if (string? prog) (read-string (str "[" (slurp prog) "]")) prog))
        byte-arr  (into-array Byte/TYPE (map #(byte (if (>= % 128) (- % 256) %)) assembled))]
    (with-open [f (java.io.FileOutputStream. out)]
      (.write f (into-array Byte/TYPE (map #(byte (if (>= % 128) (- % 256) %)) assembled)))
      nil)))

(defn run! [prog]
  (let [filename "/tmp/a.com"]
    (assemble-file prog filename)
    (sh "dosbox" filename)
    nil))
