(ns clojure.contrib.sparsearray
  (:import dke.OpenBitsetSparseArray))

(defn sparse-array [capacity]
  (OpenBitsetSparseArray. capacity))

(defn sparse-array-clear! [a]
  (.clear a))

(defn sparse-array-put! [a i obj]
    (.put a i obj))

(defn sparse-array-get [a i]
  (.get a i))

(defn sparse-array-indexOf [a i]
  (.indexOf a i))

(defn sparse-array-size [a]
  (.size a))
