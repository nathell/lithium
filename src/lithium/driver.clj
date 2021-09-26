(ns lithium.driver
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]))

(defn run-and-wait! [prog]
  (let [binary "/tmp/runwait.com"]
    (with-open [f (java.io.FileOutputStream. binary)]
      (.write f (byte-array prog))
      (sh "dosbox" binary)
      nil)))

(defn parse-registers [str]
  (->> (string/split str #"[\r\n]+")
       (map string/trim)
       (filter seq)
       (map (fn [s]
              (let [[reg val] (string/split s #"=")]
                [(keyword (string/lower-case reg))
                 (Long/parseLong val 16)])))
       (into {})))

(defn run-and-capture-registers! [prog]
  (let [tmpdir "/tmp"
        binary (str tmpdir "/runcapt.com")
        batch (str tmpdir "/run.bat")
        output (str tmpdir "/REGS.TXT")]
    (with-open [f (java.io.FileOutputStream. binary)]
      (.write f (byte-array prog)))
    (spit batch "@cls\n@runcapt.com >regs.txt\n@exit\n")
    (sh "dosbox" batch :env {"SDL_VIDEODRIVER" "dummy"})
    (parse-registers (slurp output))))

(defn run-program! [prog wait?]
  ((if wait? run-and-wait! run-and-capture-registers!) prog))

(defn hexdump [bytes]
  (string/join " " (map #(format "%02x" %) bytes)))
