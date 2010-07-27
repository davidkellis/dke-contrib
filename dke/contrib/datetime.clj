(ns dke.contrib.datetime
  (:use [dke.contrib.core :only (str-to-int)]
        [clojure.contrib.math :only (floor)])
  (:import [org.joda.time DateTime Period Duration Interval LocalTime DateTimeConstants]
           [java.io PrintWriter])
  (:require [clojure.contrib.json :as json]))

; Writes a string representation of an arbitrary object, obj, to the PrintWriter, out.
; This function is for use with the Write-JSON protocol.
(defn write-json-toString [obj #^PrintWriter out]
  (.print out (str obj)))

; Make the DateTime class participate in the Write-JSON protocol.
(extend org.joda.time.DateTime
  json/Write-JSON
  {:write-json write-json-toString})

(extend org.joda.time.Period
  json/Write-JSON
  {:write-json write-json-toString})

(extend org.joda.time.Duration
  json/Write-JSON
  {:write-json write-json-toString})

(extend org.joda.time.Interval
  json/Write-JSON
  {:write-json write-json-toString})

(extend org.joda.time.LocalTime
  json/Write-JSON
  {:write-json write-json-toString})

(extend org.joda.time.DateTimeConstants
  json/Write-JSON
  {:write-json write-json-toString})

(defn split-ts-date [date]
  (vec (map str-to-int [(subs date 0 4)
                        (subs date 4 6)
                        (subs date 6 8)])))

(defn split-ts-time [time]
  (vec (map str-to-int [(subs time 0 2)
                        (subs time 2 4)
                        (subs time 4 6)])))

(defn ts-to-date-time [ts]
  [(subs ts 0 8) (subs ts 8 14)])

(defn split-ts [ts]
  (let [[date time] (ts-to-date-time ts)]
    (vec (concat (split-ts-date date) (split-ts-time time)))))

(defn timestamp [#^DateTime dt]
  (.toString dt "yyyyMMddHHmmss"))

(defn datetime
  ([]
    (DateTime.))
  ([ts]
    (apply datetime (split-ts ts)))
  ([y m d H M S]
    (DateTime. y m d H M S 0)))

(defn duration [millis]
  (Duration. (long millis)))

(defn period [y m d H M S]
  ; Constructor: Period(int years, int months, int weeks, int days, int hours, int minutes, int seconds, int millis)
  (Period. y m 0 d H M S 0))

(defn period-between [#^DateTime t1 #^DateTime t2]
  (Period. t1 t2))

(defn duration-between [#^DateTime t1 #^DateTime t2]
  (Duration. t1 t2))

(defn interval-between [#^DateTime t1 #^DateTime t2]
  (Interval. t1 t2))

(defn date-pair [#^Interval interval]
  [(.getStart interval) (.getEnd interval)])

(defn increment-time [#^DateTime instant #^Period increment]
  (.plus instant increment))

(defn decrement-time [#^DateTime instant #^Period decrement]
  (.minus instant decrement))

; Returns true if instant1 is before instant2
; i.e. instant1 < instant2 ?
(defn before? [#^DateTime instant1 #^DateTime instant2]
  (.isBefore instant1 instant2))

; Returns true if instant1 is after instant2
; i.e. instant1 > instant2 ?
(defn after? [#^DateTime instant1 #^DateTime instant2]
  (.isAfter instant1 instant2))

; t1 <= instant <= t2
(defn instant-between-inclusive? [instant t1 t2]
  (and t1 t2 (not (before? instant t1)) (not (after? instant t2))))

; t1 <= instant < t2
(defn instant-between? [#^DateTime instant t1 t2]
  (and t1 t2 (.contains #^Interval (interval-between t1 t2) instant)))

; duration1 > duration2
; (defmulti longer-than? (fn [& args] (vec (map class args))))
; (defmethod longer-than? [Duration Duration] [duration1 duration2]
;   (.isLongerThan duration1 duration2))
; (defmethod longer-than? [Interval Interval] [interval1 interval2]
;   (.isLongerThan (.toDuration duration1) (.toDuration duration2)))

; timespan1 > timespan2
; timespan1 and timespan2 must be instances of Duration or Interval
(defn longer-than? [timespan1 timespan2]
  (let [#^Duration timespan1 (if (instance? Interval timespan1)
                               (.toDuration #^Interval timespan1)
                               timespan1)
        #^Duration timespan2 (if (instance? Interval timespan2)
                               (.toDuration #^Interval timespan2)
                               timespan2)]
    (.isLongerThan timespan1 timespan2)))

; timespan1 < timespan2
; timespan1 and timespan2 must be instances of Duration or Interval
(defn shorter-than? [timespan1 timespan2]
  (let [#^Duration timespan1 (if (instance? Interval timespan1)
                               (.toDuration #^Interval timespan1)
                               timespan1)
        #^Duration timespan2 (if (instance? Interval timespan2)
                               (.toDuration #^Interval timespan2)
                               timespan2)]
    (.isShorterThan timespan1 timespan2)))

(defn max-datetime
  ([dt-seq]
    (reduce max-datetime dt-seq))
  ([dt1 dt2]
    (if (after? dt1 dt2)
      dt1
      dt2)))

(defn min-datetime
  ([dt-seq]
    (reduce min-datetime dt-seq))
  ([dt1 dt2]
    (if (before? dt1 dt2)
      dt1
      dt2)))

; Assumes t1 <= t2
; Returns a random datetime between t1 (inclusive) and t2 (exclusive).
(defn random-datetime [#^DateTime t1 #^DateTime t2]
  (let [r (rand)
        #^Duration duration (duration-between t1 t2)
        ms-offset (long (bigdec (* r (.getMillis duration))))]
    (.plus t1 ms-offset)))

; returns a lazy sequence of times interspersed with equal-length time-periods
; The sequence starts with start-time and the nth subsequent time is computed as: start-time + n * time-increment
; Example: (take 5 (time-series (datetime) (org.joda.time.Period/hours 1)))
;          (take 5 (time-series (datetime) (fn [t] (.plus t (org.joda.time.Period/hours 1)))))
;          (take 5 (time-series (datetime) (fn [t] (.minus t (org.joda.time.Period/hours 1)))))
(defn time-series [start-time next-time-fn]
  (cons start-time (lazy-seq (time-series (next-time-fn start-time) next-time-fn))))

; Example: (offset-date-pair [#<DateTime 1999-04-01T08:32:00.000-06:00> #<DateTime 2009-04-01T13:42:00.000-05:00>]
;                            :after
;                            (Period/days 1)
;                            :before
;                            (Period/days 2))
;          -> [#<DateTime 1999-04-02T08:32:00.000-06:00> #<DateTime 2009-03-30T13:42:00.000-05:00>]
(defn offset-date-pair [pair start-offset-dir start-offset end-offset-dir end-offset]
  (let [start-offset-fn (cond (or (nil? start-offset-dir) (nil? start-offset))
                                (fn [dt offset] dt)
                              (= start-offset-dir :before)
                                #(.minus %1 %2)
                              :default
                                #(.plus %1 %2))
        end-offset-fn (cond (or (nil? end-offset-dir) (nil? end-offset))
                              (fn [dt offset] dt)
                            (= end-offset-dir :before)
                              #(.minus %1 %2)
                            :default
                              #(.plus %1 %2))
        [start-dt end-dt] pair
        adj-start (start-offset-fn start-dt start-offset)
        adj-end (end-offset-fn end-dt end-offset)]
    [adj-start adj-end]))

; Example: (expand-interval (interval-between (datetime "19950101000000") (datetime "20050101000000")) :before (Period/days 1) nil nil)
;          -> #<Interval 1994-12-31T00:00:00.000/2005-01-01T00:00:00.000>
(defn expand-interval [interval start-offset-dir start-offset end-offset-dir end-offset]
  (apply interval-between (offset-date-pair (date-pair interval) start-offset-dir start-offset end-offset-dir end-offset)))
