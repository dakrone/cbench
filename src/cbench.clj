;(ns org.clojars.thnetos/cbench ; which one?
(ns cbench ; which one?
  (:import (java.util Date))
  (:use [clojure.contrib.str-utils :only (str-join)]))

;(set! *warn-on-reflection* true)

(defn- timenow
  "Return the current time"
  []
  (.getTime (Date.)))

(defn- standard-deviation
  "Return the standard deviation, given a sequence of numbers. Giving an
  average is optional (if an average has already been calculated)"
  ([list-of-nums average]
   (Math/sqrt
     (/ (reduce + (map #(Math/abs (double (- % average))) list-of-nums))
        (- (count list-of-nums) 1))))
  ([list-of-nums]
     (standard-deviation list-of-nums (/ (reduce + list-of-nums) (count list-of-nums)))))
    

(defn- cbtime
  "Return the number of milliseconds required to run a function f"
  [f]
  (let [start (timenow)]
    (f)
    (float (- (timenow) start))))

(defn cbench
  "Return a sequence of benchmark values for a number of times and a function.
  Returns: [average minimum maximum standard-deviation total]"
  [times f]
  (let [functions (repeat times f)
        start     (timenow)
        timelist  (doall (map cbtime functions)) ; doall forces evaluation
        totaltime (- (timenow) start)
        avg       (/ (reduce + timelist) (count timelist))
        tmax      (apply max timelist)
        tmin      (apply min timelist)
        stddev    (standard-deviation timelist avg)]
    [avg tmax tmin stddev totaltime]))

(defn cbench-pp
  "Performs cbench, with a pretty output display"
  [times f]
  (let [[avg
         tmax
         tmin
         stddev
         totaltime] (cbench times f)]
    (println (str-join "" (repeat 35 "-")))
    (println (str "| avg    | " (float avg) " ms"))
    (println (str "| min    | " tmin " ms"))
    (println (str "| max    | " tmax " ms"))
    (println (str "| stddev | " stddev " ms"))
    (println (str "| total  | " totaltime " ms"))
    (println (str-join "" (repeat 35 "-")))
    ))


(comment

  (cbench 50 #(Thread/sleep 15))

  (cbench-pp 50 #(Thread/sleep 15))

  ; Example: Compare clojure abs and Math/abs

  (defn abs
  "Returns the absolute value of x"
  [#^Double x]
  (if (neg? x)
  (- x)
  x))

  (cbench-pp 100000 #(abs (- 50 (rand-int 100))))
  (cbench-pp 100000 #(Math/abs (double (- 50 (rand-int 100)))))

)
