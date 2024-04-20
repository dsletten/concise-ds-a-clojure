;;;;
;;;;
;;;;   Programming is not about typing, it's about thinking.
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               yfi.clj
;;;;
;;;;   Started:            Wed Jul 19 02:25:21 2023
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
  (length [this])
  (add [this that])
  (equal [this that]))

(deftype YFI [inches]
  Measure
  (length [this] inches)
  (add [this that]
    (YFI. (clojure.core/+ (length this) (length that))))
  (equal [this that]
    (clojure.core/== (length this) (length that))))
  ;; Object
  ;; (equals [this that]
  ;;   (clojure.core/= (length this) (length that))))

(extend-protocol Measure
  ;; clojure.lang.Symbol
  ;; (length [this] (println "S:" this) this)
  Long
  (length [this] this)
  (add [this that]
    (YFI. (clojure.core/+ this (length that))))
  (equal [this that]
    (clojure.core/== this (length that))))
  ;; Object
  ;; (equals [this that]
  ;;   (clojure.core/= this (length that))))
    
(let [zero (YFI. 0)]
  (defn + [& yfis]
    (reduce add zero yfis)))

(defn == [yfi & yfis]
  (every? (partial equal yfi) yfis))

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

(defn make-yfi [& args]
  (let [construct (fn
                    ([o]
                     (println ">" o)
                     ;; (case (class o)
                     ;;   concise.yfi.YFI o
                     ;;   (YFI. o))))
                     (if (instance? YFI o)
                       o
                       (YFI. o)))
                    ([feet inches]
                     (println ">>" [feet inches])
                     (YFI. (clojure.core/+ (feet->inches feet) inches)))
                    ([yards feet inches]
                     (println ">>>" [yards feet inches])
                     (YFI. (clojure.core/+ (yards->inches yards)
                                           (feet->inches feet)
                                           inches)))
                    ([_ _ _ & _] (throw (IllegalArgumentException.
                                         (format "Invalid initialization: %s" args)))))]
    (cond (nil? args)
          (YFI. 0)
          (every? (every-pred integer? (complement neg?)) args)
          (apply construct args)
          :else
          (throw (IllegalArgumentException. "Length components must be non-negative integers.")))))

;; (defmethod print-method YFI [yfi writer]
;;   (.write writer (format "%s yards: %d feet: %d inches: %d"
;;                          (.getSimpleName (class yfi))
;;                          (yards yfi)
;;                          (feet yfi)
;;                          (inches yfi))))
                         
(defmethod print-method YFI [yfi writer]
  (.write writer (format "#yfi \"yards: %d feet: %d inches: %d\""
                         (yards yfi)
                         (feet yfi)
                         (inches yfi))))

(defn read-yfi [^CharSequence cs]
  (if-let [[_ yards feet inches] (re-matches #"yards: (\d+) feet: (\d+) inches: (\d+)" cs)]
    (apply make-yfi (map #(Long/parseLong %) [yards feet inches]))
    (throw (RuntimeException. "Unrecognized YFI"))))
