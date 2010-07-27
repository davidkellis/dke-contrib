(ns dke.contrib.math
  (:use [clojure.contrib.math :only (abs expt sqrt)])
  (:import java.lang.Math))

; PI, out to 30 decimal digits, taken from http://en.wikipedia.org/wiki/Numerical_approximations_of_%CF%80
(def PI (rationalize 3.141592653589793238462643383279M))

; E, out to 30 decimal digits, taken from http://www.gutenberg.org/cache/epub/127/pg127.html
(def E (rationalize 2.718281828459045235360287471352M))

(def hundredth 0.01)
(def thousandth 0.001)
(def millionth 0.000001)
(def billionth 0.000000001)

(defn neg [n]
  (- 0 n))

(defn sign-same? [a b]
  (= (neg? a) (neg? b)))

(defn sign-diff? [a b]
  (not (= (neg? a) (neg? b))))

; This is a recursive implementation of the Bisection Method as defined by pseudocode in
; Bisection3 on page 95 of Numerical Mathematics and Computing (5th ed.) by Ward/Kincaid.
;
; Arguments:
; a, b define the interval within which the root is guaranteed to exist (i.e. a < root < b)
; fa, fb are f(a) and f(b) respectively
; n-max is the maximum iterations to perform
; epsilon is an error threshold. The algorithm continues iterating until the error is less than epsilon.
; n is the current iteration
;
; Returns:
; [root-approximation error number-of-iterations]
(defn bisection-method
  ([f a b]
    (bisection-method f a b (f a) (f b) 300 billionth 1))
  ([f a b n-max epsilon]
    (bisection-method f a b (f a) (f b) n-max epsilon 1))
  ([f a b n-max epsilon n]
    (bisection-method f a b (f a) (f b) n-max epsilon n))
  ([f a b fa fb n-max epsilon n]
    (let [error (/ (- b a) 2)     ; error <- (b - a) / 2
          c (+ a error)           ; c <- a + error  (c is the midpoint between a and b ; this is our best root approximation)
          fc (f c)                ; fc <- f(c)      (fc is f evaluated at the midpoint between a and b)
          n (inc n)]
      (if (or (< (abs error) epsilon)   ; our error is less than the error threshold epsilon (i.e. we have converged enough, so return c)
              (> n n-max))              ; we've executed the maximum number of iterations (i.e. we have converged enough, so return c)
        [c error (dec n)]
        (if (sign-diff? fa fc)
          (recur f a c fa fc n-max epsilon n)
          (recur f c b fc fb n-max epsilon n))))))

; Returns the nth root of A
; -> A^(1/n)
; It works by finding the positive root (i.e. positive zero) of the function f(x) = x^n - A
; It returns the positive x at which f(x) = 0.
;
; Arguments:
; n is the root we want to find (i.e. the "n" in "nth root")
; A is the positive real number that we want to find the nth root of.
;
; Usage:
;   (nth-root 45.13579 3)
;   -> 3.5604674194663124
;   Check the result:
;   (expt 3.5604674194663124 3)
;   45.13578999552352        (that's pretty close!)
;
;   (nth-root 0.456 4)
;   -> 882350387/1073741824       ; (float (nth-root 0.456 4)) = 0.82175285
;   Check the result:
;   (float (expt 882350387/1073741824 4))
;   -> 0.456
(defn nth-root [A n]
  (let [f (fn [x] (- (expt x n) A))]
    (if (< A 1)
      (first (bisection-method f 0 1))
      (first (bisection-method f 0 A)))))

; Approximate the definite integral of f from a to b, using Composite Simpson's rule
; This is a Clojure port of the Python implementation at http://en.wikipedia.org/wiki/Simpson's_rule
(defn simpson-integrate
  ([f a b]
    (simpson-integrate f a b 100))
  ([f a b n]
    (let [h (/ (- b a) n)       ; factor this out of the loop for efficiency
          sum (reduce +
                (map (fn [k]
                       (* (inc (rem k 2))
                          (f (+ a (* h k)))))
                     (range 1 n)))]
      (* (/ h 3)
         (+ (f a)
            (* 2 sum)
            (f b))))))

; taken from clojure.org documentation
(defn factorial [n]
  (loop [cnt n acc 1]
    (if (zero? cnt)
      acc
      (recur (dec cnt) (* acc cnt)))))

(def factorial (memoize factorial))

; http://en.wikipedia.org/wiki/Taylor_series
; Implementation of Euler's Exponential function (Maclaurin series expansion):
; e^x = summation_0_infinity[x^n / n!] = 1 + x + (x^2)/2! + (x^3)/3! + ... for all x
(defn exp
  ([x]
    (exp x 50))
  ([x term-count]
    (reduce +
            (map (fn [n]
                   (/ (expt x n)
                      (factorial n)))
                 (range term-count)))))

; returns e^x
(defn exp2 [x]
  (expt E x))

; This function approximates the integrand of the Gamma function.
; http://en.wikipedia.org/wiki/Gamma_function
(defn gamma-integrand
  ([z x]
    (* (expt x (dec z))
       (/ 1 (exp2 x))))
  ([z exp-approx-terms x]
    (* (expt x (dec z))
       (/ 1 (exp x exp-approx-terms)))))

; This function is an approximation of the Gamma function.
; http://en.wikipedia.org/wiki/Gamma_function
(defn gamma
  ([z]
    (gamma z 60))
  ([z int-upper-bound]
    (simpson-integrate
      (partial gamma-integrand z)
      0
      int-upper-bound
      150))
  ([z int-upper-bound int-approx-terms]
    (simpson-integrate
      (partial gamma-integrand z)
      0
      int-upper-bound
      int-approx-terms))
  ([z exp-approx-terms int-upper-bound int-approx-terms]     ; we want int-upper-bound to be infinity, but we use some large number to get a fairly close approximation
    (simpson-integrate
      (partial gamma-integrand z exp-approx-terms)
      0
      int-upper-bound
      int-approx-terms)))

(defn student-t-pdf
  ([df]
    (let [a (/ (inc df) 2)
          gamma-num (gamma a)
          gamma-denom (gamma (/ df 2))]
      (fn [t]
        (* (/ gamma-num
              (* (sqrt (* df PI)) gamma-denom))
           (/ 1 (expt (+ 1 (/ (expt t 2) df))
                      a))))))
  ([df t]
    (let [a (/ (inc df) 2)
          b (/ df 2)]
      (* (/ (gamma a)
            (* (sqrt (* df PI)) (gamma b)))
         (/ 1 (expt (+ 1 (/ (expt t 2) df))
                    a))))))

(defn student-t-cdf
  ([df t]
    (student-t-cdf df t -50 100))
  ([df t int-lower-bound]
    (student-t-cdf df t int-lower-bound 100))
  ([df t int-lower-bound int-approx-terms]      ; we want int-lower-bound to be -infinity, but we use some large negative number to get a fairly close approximation
    (let [pdf (student-t-pdf df)]
      (simpson-integrate
        pdf
        int-lower-bound
        t
        int-approx-terms))))

; cl is a confidence level
; Example: (root-of-student-t-cdf 6 0.95)
;          -> 
(defn root-of-student-t-cdf [df cl]
  (let [cdf (partial student-t-cdf df)
        f (fn [t] (- (cdf t) cl))]
    (first (bisection-method f 0 50))))


; This is a utility function used by two_tailed_p_score, ported from a javascript implementation found in the source code of:
; http://home.ubalt.edu/ntsbarsh/Business-stat/otherapplets/pvalues.htm#rtdist
(defn stat-com [q i j b]
  (loop [zz 1
         z 1
         k i]
    (if (> k j)
      z
      (let [zz2 (/ (* zz q k) (- k b))]
        (recur zz2 (+ z zz2) (+ k 2))))))

; This function, two-tailed-cdf, and stat-com are magic. I don't know why they work.
; Compute the two-tailed p score given a t-statistic and degrees-of-freedom
; Javascript implementation of this function found in the source code of:
;   http://home.ubalt.edu/ntsbarsh/Business-stat/otherapplets/pvalues.htm#rtdist
; I believe this is a numerical method of computing the integral that evaluates to the area under the t distribution curve,
;   given the t-statistic and degrees-of-freedom.
; Returns alpha = (1 - confidence_level)
(defn two-tailed-cdf [df t]
  (let [t (abs t)
        w (/ t (sqrt df))
        th (Math/atan w)]
    (if (= df 1)
      (- 1 (/ th (/ PI 2)))
      (let [sth (Math/sin th)
            cth (Math/cos th)]
        (if (= (rem df 2) 1)
          (- 1 (/ (+ th (* sth cth (stat-com (expt cth 2)
                                             2
                                             (- df 3)
                                             -1)))
                  (/ PI 2)))
          (- 1 (* sth (stat-com (expt cth 2)
                                1
                                (- df 3)
                                -1))))))))

(defn one-tailed-cdf [df t]
  (/ (two-tailed-cdf df t) 2))

; cl is a confidence level
(defn root-of-cdf [cdf df cl]
  (let [cdf (partial cdf df)
        alpha (- 1 cl)
        f (fn [t] (- (cdf t) alpha))]
    (first (bisection-method f 0 1000))))


