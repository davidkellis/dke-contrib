(ns dke.contrib.statistics
  (:use [clojure.contrib.math :only (expt sqrt)]
        [dke.contrib.math :only (nth-root neg thousandth millionth billionth)]
        [dke.contrib.dkeutils :only (multi-assoc-in)]))

(defn summation [sequence]
  (reduce + sequence))

(defn product [sequence]
  (reduce * sequence))

; Example: (mean [1 2 3 4.4 5.5 6.6 13])
;          -> 5.071428571428571
;          (mean [1 2 3 4])
;          -> 5/2
(defn mean [sequence]
  (let [c (count sequence)]
    (if (> c 0)
      (/ (summation sequence) c))))

(defn differences-from-mean
  ([sequence]
    (differences-from-mean sequence (mean sequence)))
  ([sequence avg]
    (map #(- % avg) sequence)))

(defn sum-of-squares
  ([sequence]
    (summation (map #(expt % 2) (differences-from-mean sequence))))
  ([sequence avg]
    (summation (map #(expt % 2) (differences-from-mean sequence avg)))))

(defn population-variance
  ([sequence]
    (let [n (count sequence)]
      (if (= n 0)
        0
        (/ (sum-of-squares sequence) n))))
  ([sequence avg]
    (let [n (count sequence)]
      (if (= n 0)
        0
        (/ (sum-of-squares sequence avg) n)))))

(defn sample-variance [sequence]
  (let [adj-length (- (count sequence) 1)]
    (if (<= adj-length 0)
      0
      (/ (sum-of-squares sequence) adj-length))))

(defn population-std-dev
  ([sequence]
    (sqrt (population-variance sequence)))
  ([sequence avg]     ; this is not faster than the one-parameter version
    (sqrt (population-variance sequence avg))))

(defn sample-std-dev [sequence]
  (sqrt (sample-variance sequence)))

(defn sample-std-err [sequence]
  (let [n (count sequence)]
    (if (> n 0)
      (/ (sample-std-dev sequence) (sqrt n)))))

(defn sample-t-statistic [sequence null-hypothesis-mean]
  (/ (- (mean sequence) null-hypothesis-mean)
     (sample-std-err sequence)))



(defn sample-normal-distribution [s]
  {:count (count s)
   :mean (mean s)
   :variance (sample-variance s)})

(defn distribution-std-dev [d]
  (sqrt (:variance d)))

(defn combine-distribution-means [d1 d2]
  (let [{n1 :count mean1 :mean} d1
        {n2 :count mean2 :mean} d2]
    (/ (+ (* n1 mean1) (* n2 mean2))
       (+ n1 n2))))

; Assumes that the distributions are of non-overlapping samples (i.e. the distributions are independent).
(defn combine-sample-variances [d1 d2]
  (let [{nx :count mx :mean vx :variance} d1
        {ny :count my :mean vy :variance} d2
        mxy (combine-distribution-means d1 d2)]
    (/ (- (+ (* (- nx 1) vx) (* nx (expt mx 2)) (* (- ny 1) vy) (* ny (expt my 2))) (* (+ nx ny) (expt mxy 2)))
       (+ nx ny -1))))

; Assumes that the distributions are of non-overlapping samples (i.e. the distributions are independent).
(defn combine-sample-normal-distributions
  ([distribution-seq]
    (reduce combine-sample-normal-distributions distribution-seq))
  ([dist1 dist2]
    {:count (+ (:count dist1) (:count dist2))
     :mean (combine-distribution-means dist1 dist2)
     :variance (combine-sample-variances dist1 dist2)}))


; Simple Moving Average - SMA
;
; Arguments:
; s - a sequence of observations, having length >= n ; the first item is the most recent
; n - the number of n-periods to use in the computation of the average.
;   - the number of terms used to compute this sma (e.g. (sma [3 4 5 6 7 8 9] 4) uses 4 terms => n = 4).
; prev-sma - the sma one time period ago ; sma at t-1
; prev-s - the sequence of observations used to compute the previous sma (prev-sma) ; s at t-1
;        - (last s) == (nth prev-s (dec n))
;        - the last observation is the one that we want to remove from the previous sma (prev-sma).
;        - since the observation represents the numerator, we have to divide the oldest observation by n
;          to represent the term that we want to subtract from prev-sma
;
; Returns a vector pair containing the n-period mean and the nth item (the last item) from the sequence used in the mean.
;   [mean s[n-1]]
(defn sma [s n]
    (mean (take n s)))

(defn sma-update [s n prev-sma prev-s]
  (if (and prev-sma prev-s)
    (let [oldest-obs (nth prev-s (dec n))
          term-to-remove (/ oldest-obs n)
          term-to-add (/ (first s) n)]
      (+ (- prev-sma term-to-remove) term-to-add))
    (sma s n)))

; Arguments:
; n - the number of n-periods to use in the computation of the average
; W - the total weight threshold. 0 < W < 1
;
; Implementation notes:
; http://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average
; The computation of alpha is a function of n and W. We compute alpha with the following:
;   (1-alpha)^n < (1-W)
;   => ((1-alpha)^n) ^ (1/n) < (1-W)^(1/n)
;   => (1-alpha) < (1-W)^(1/n)
;   => -alpha < (1-W)^(1/n)-1
;   => alpha > -(1-W)^(1/n)+1
;   => alpha ~= -(1-W)^(1/n)+1 + 0.000000001
(defn ema-alpha [n W]
  (+ (neg (nth-root (- 1 W) n)) 1 billionth))

; Implementation of Exponential Moving Average (EMA) or Exponentially Weighted Moving Average (EWMA)
; http://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average
; I'm using "An alternate approach by Roberts (1959) uses Yt in lieu of Yt−1[3]:"
; S_t = alpha * Y_t + (1 - alpha) * S_t-1
;
; Arguments:
; s - a sequence of observations, having length >= n.
;     The first element of the sequence is weighted the most and the nth element is weighted the least.
; n - the number of n-periods to use in the computation of the average
; alpha - the smoothing constant. 0 < alpha <= 1
(defn ema
  ([s n]
    (ema s n (ema-alpha n (- 1 millionth))))      ; we want to capture all but 1 millionth of the total weight
  ([s n alpha]
    (let [alpha-comp (- 1 alpha)]
      (loop [Y s
             t n
             exp 1
             S_t (* alpha (first Y))]
        (if (= t 1)
          S_t
          (let [restY (rest Y)]
            (recur restY
                   (dec t)
                   (inc exp)
                   (+ S_t (* alpha (expt alpha-comp exp) (first restY))))))))))

(defn ema-r
  ([s n]
    (ema-r s n (ema-alpha n (- 1 millionth))))      ; we want to capture all but 1 millionth of the total weight
  ([s n alpha]
    (if (= n 1)
      (* alpha (first s))
      (+ (* alpha (first s))
         (* (- 1 alpha)
            (ema-r (rest s) (dec n) alpha))))))

(defn ema-update [s alpha prev-ema]
  (+ (* alpha (first s))
     (* (- 1 alpha) prev-ema)))

(defn ema-update-obs [obs alpha prev-ema]
  (+ (* alpha obs)
     (* (- 1 alpha) prev-ema)))

; ******************************* DISASTER BELOW *******************************

; Example: (get-meta (var myfunction) :author "David Ellis")
(defn get-meta [v key default]
  (or (key (meta v)) default))

(defn fn-dependencies [v]
  (get-meta v :dependencies []))

; Example: (running-stats [running-mean] (range 1000000))
(defn running-stats [fns c]
  (loop [s c
         current-state (reduce (fn [state f] (f state)) {} fns)]
    (if (seq s)
      (let [value (first s)]
        (recur (rest s)
               (reduce (fn [state f] (f current-state state value)) {} fns)))
      current-state)))

(defn running-count
  ([cstate]
    (assoc cstate :count 0))
  ([pstate cstate new-value]
    (assoc cstate :count (inc (:count pstate)))))

(defn running-summation
  ([cstate]
    (assoc cstate :sum 0))
  ([pstate cstate new-value]
    (assoc cstate :sum (+ (:sum pstate) new-value))))

(defn running-mean
  ([cstate]
    (merge (reduce (fn [state f] (f state)) cstate [running-count running-summation])
           {:mean 0}))
  ([pstate cstate new-value]
    (let [cstate2 (reduce (fn [state f] (f pstate state new-value)) cstate [running-count running-summation])
          n (:count cstate2)
          sum (:sum cstate2)]
      (if (= n 0)
        (assoc cstate2 :mean 0)
        (assoc cstate2 :mean (/ sum n))))))
