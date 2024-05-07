;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               yfi.clj
;;;;
;;;;   Started:            Sat May  4 17:46:33 2024
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(ns concise.yfi
  (:refer-clojure :exclude [+ ==]))

(defprotocol Measure
  (length [this]))

(deftype YFI [inches]
  Measure
  (length [this] inches))

(let [zero (YFI. 0)]
  (defn + [& yfis]
    (reduce #(YFI. (clojure.core/+ (length %1) (length %2))) zero yfis)))

;; (let [zero (YFI. 0)]
;;   (defn + [& yfis]
;;     (reduce (comp #(YFI. %)
;;                   (comp (partial apply clojure.core/+)
;;                         (partial map length) list))
;;             zero
;;             yfis)))

(defn == [yfi & yfis]
  (every? #(clojure.core/== (length yfi) (length %)) yfis))

;; (defn == [yfi & yfis]
;;   (every? (comp (partial clojure.core/== (length yfi)) length) yfis))

(defn inches [yfi]
  (mod (length yfi) 12))

(defn feet [yfi]
  (mod (long (/ (length yfi) 12)) 3))

(defn yards [yfi]
  (long (/ (length yfi) 36)))

(defn- feet->inches [feet]
  (* feet 12))

(defn- yards->inches [yards]
  (* yards 36))

(defn make-yfi
  ([] (println "|") (make-yfi 0 0 0))
  ([inches]
   (println ">" inches)
   (make-yfi 0 0 inches))
  ([feet inches]
   (println ">>" [feet inches])
   (make-yfi 0 feet inches))
  ([yards feet inches]
   (println ">>>" [yards feet inches])
   (if (every? (every-pred integer? (complement neg?)) [yards feet inches])
     (YFI. (clojure.core/+ (yards->inches yards)
                           (feet->inches feet)
                           inches))
     (throw (IllegalArgumentException. "Length components must be non-negative integers.")))))

(defmethod print-method YFI [yfi writer]
  (.write writer (format "#yfi \"yards: %d feet: %d inches: %d\""
                         (yards yfi)
                         (feet yfi)
                         (inches yfi))))

(defn read-yfi [^CharSequence cs]
  (if-let [[_ yards feet inches] (re-matches #"yards: (\d+) feet: (\d+) inches: (\d+)" cs)]
    (apply make-yfi (map #(Long/parseLong %) [yards feet inches]))
    (throw (RuntimeException. "Unrecognized YFI"))))

;; (+ #yfi "yards: 0 feet: 2 inches: 5" #yfi "yards: 1 feet: 2 inches: 5")
;; #yfi "yards: 2 feet: 1 inches: 10"


;; (count-if #'evenp '(1 2 3 4 5)) => 2

(defn count-if [f coll]
  ((comp count filter) f coll))

(count-if even? [1 2 3 4 5])

;; (f x y) -> (f (g x) (g y))
;; (* 1 2) -> (* (inc 1) (inc 2))

;; (comp (partial apply f) (partial map g) list)

(comp (partial apply *) (partial map inc) list)
((comp (partial apply *) (partial map inc) list) 1)
((comp (partial apply *) (partial map inc) list) 1 2)
((comp (partial apply *) (partial map inc) list) 1 2 3)
