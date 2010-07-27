(ns dke.contrib.string
  (:use [clojure.contrib.string :only (split)]))

(defn split-on [s split-string]
  (split (re-pattern split-string)
         s))