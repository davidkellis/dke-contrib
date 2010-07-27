(ns dke.contrib.csv
  (:use [com.davidsantiago.csv :only (write-csv)]
        [clojure.contrib.io :only (spit)]
        [clojure.contrib.string :only (split-lines)]
        [dke.contrib.string :only (split-on)]))

(defn parse-simple-csv [s]
  (try
    (map #(vec (split-on % ",")) (split-lines s))
    (catch java.lang.RuntimeException _ [])))

; Return a lazy sequence of rows.
; Each row is a vector of fields, each of which is a string.
; Example return: ([1 2 3] [4 5 6] [7 8 9])
(defn read-csv [filename]
  (parse-simple-csv (slurp filename)))
  ; (parse-csv (slurp filename)))

(defn write-csv-file [filename table]
  (spit filename (write-csv table)))
