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
  (->> (string/split str #"\r\n")
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
        screen-grab (str tmpdir "/TEXT.VID")]
    (when (.exists (io/file screen-grab))
      (io/delete-file screen-grab))
    (with-open [f (java.io.FileOutputStream. binary)]
      (.write f (byte-array prog)))
    (with-open [in (io/input-stream (io/resource "capture.com"))]
      (io/copy in (io/file (str tmpdir "/capture.com"))))
    (spit batch "@cls\n@runcapt.com\n@capture.com t\n@exit\n")
    (sh "dosbox" batch :env {"SDL_VIDEODRIVER" "dummy"})
    (parse-registers (slurp screen-grab))))

(defn run-program! [prog wait?]
  ((if wait? run-and-wait! run-and-capture-registers!) prog))

(defn hexdump [bytes]
  (string/join " " (map #(format "%02x" %) bytes)))
