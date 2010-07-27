(ns dke.contrib.dkeutils
  (:use [dke.contrib.filesystem :only (path-filename canonical-file-path)]
        [clojure.contrib.seq :only (indexed)]
        [clojure.contrib.math :only (floor)])
  (:import java.io.File))

; each ksvp (key-set/value pair) is of the form: [key1 key2 key3 ...] value
; Example:
;   (multi-assoc-in {:cash 50 :securities {:AAPL 10}} [[:cash] 10 [:securities :BBBB] 30])
; => {:cash 10, :securities {:BBBB 30, :AAPL 10}}
(defn multi-assoc-in [m [ks v & ksvps]]
  (if (nil? ksvps)
    (assoc-in m ks v)
    (recur (assoc-in m ks v) ksvps)))

; similar to multi-assoc-in, except that it behaves like update-in instead of assoc-in
(defn multi-update-in [m [ks f & ksvps]]
  (if (nil? ksvps)
    (update-in m ks f)
    (recur (update-in m ks f) ksvps)))

; returns a lazy seq of the values in map concatenated together
; Example: (concat-vals {:a [1 2 3] :b [4 5 6] :c [7 8 9]})
;          -> (1 2 3 4 5 6 7 8 9)
(defn concat-vals [m]
  (apply concat (vals m)))

; http://blog.thinkrelevance.com/2009/8/12/rifle-oriented-programming-with-clojure-2
(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn run-as-script? []
  ; (= (first *command-line-args*)
  ;    (last (split *file* (re-pattern File/separator)))))
  (let [script-filename (or (and *command-line-args*
                                 (first *command-line-args*))
                            "")]
    (= (canonical-file-path script-filename)
       (canonical-file-path *file*))))

(defn tupleize [& sequences]
  (apply map (cons vector sequences)))

; right-partial
(defn rpartial [f & boundargs]
  (fn [& args]
    (apply f (concat args boundargs))))

; Do any of the functions return true given args?
; fns is a collection of functions, each of arity (count args)
; args are passed to each function in fn
(defn any? [pred-fns & args]
  (or (some (fn [f] (apply f args)) pred-fns) false))

; Do all of the functions return true given args?
; fns is a collection of functions, each of arity (count args)
; args are passed to each function in fn
(defn all? [pred-fns & args]
  (every? (fn [f] (apply f args)) pred-fns))

(defn bsearch
  ([a e]
    (bsearch a e 0 (dec (count a))))
  ([a e l h]
    (if (not (< h l))
      (let [m (quot (+ l h) 2)
            o (a m)]
        (cond (> e o) (recur a e (inc m) h)
              (< e o) (recur a e l (dec m))
              (= e o) m)))))

(defmacro form-to-string [form]
  (str form))

(defn fname [f]
  (if-let [m (meta f)]
    (m :name)
    ""))

; Examples:
;   (take 10 (rand-nth-seq ["A" "B" "C" "D"]))
;   -> ("B" "B" "B" "D" "C" "C" "A" "A" "D" "D")
;   (rand-nth-seq 10 ["A" "B" "C" "D"])
;   -> ("C" "B" "B" "B" "B" "A" "C" "D" "A" "D")
(defn rand-nth-seq
  ([s]
    (let [rand-element #(rand-nth s)]
      (repeatedly rand-element)))
  ([n s]
    (take n (rand-nth-seq s))))

; returns an int in the range of [start, end]
(defn rand-int-between [start end]
  (if (>= end start)
    (let [diff (- end start)
          offset (rand-int (inc diff))]
      (+ start offset))
    (let [diff (- start end)
          offset (rand-int (inc diff))]
      (+ end offset))))

; returns a real in the range of [start, end)
(defn rand-real-between [start end]
  (if (>= end start)
    (let [diff (- end start)
          offset (* diff (rand))]
      (+ start offset))
    (let [diff (- start end)
          offset (* diff (rand))]
      (+ end offset))))

; Returns a value, n, such that (s.t.):
;   (base + low) <= n <= (base + high)
; The return value, n, will not be less than minimum-value.
;
; Arguments:
;   base - the base number with which we add/subtract a randomly chosen offset
;   low - the minimum possible offset
;   high - the maximum possible offset
;   minimum-value - the smallest possible return value (this puts a floor on the return value),
;                   thus skewing the distribution of values (i.e. they will no longer be random).
;
; Usage:
;   (rand-int-offset 5 -2 2)        ; the range is [3, 7]
;   -> 6
;
;   (rand-int-offset 5 0 5)         ; range is [5, 10]
;   -> 6
;
;   (rand-int-offset 5 0 5 7)         ; range is [7, 10]
;   -> 7
(defn rand-int-offset
  ([base low high]
    (+ base (rand-int-between low high)))
  ([base low high minimum-value]
    (max (+ base (rand-int-between low high))
         minimum-value)))

; Usage:
;   (take 10 (moving-subseq [1 2 3 4 5 6]))
;   -> ([1 2 3 4 5 6] (2 3 4 5 6) (3 4 5 6) (4 5 6) (5 6) (6) () () () ())
(defn moving-subseq [s]
  (iterate #(drop 1 %) s))    ; note: (take 10 (iterate #(drop 1 %) [1 2 3 4 5 6])) -> ([1 2 3 4 5 6] (2 3 4 5 6) (3 4 5 6) (4 5 6) (5 6) (6) () () () ())

; Usage:
;   (take 10 (moving-subseq [1 2 3 4 5 6]))
;   -> ([1 2 3 4 5 6] [2 3 4 5 6] [3 4 5 6] [4 5 6] [5 6] [6] [] [] [] [])
(defn moving-subvec [v]
  (map vec (iterate #(drop 1 %) v)))