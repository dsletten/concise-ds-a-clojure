;;;;
;;;;
;;;;   One of the nice things about Clojure is that it lets you fix Java
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               cyclic_counter.clj
;;;;
;;;;   Started:            Wed Jun 28 00:59:52 2023
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

(ns concise.cyclic-counter
  (:refer-clojure :exclude [set]))

(defprotocol Counter
  (index [this])
  (modulus [this])
  (advance [this] [this n])
  (set [this n])
  (reset [this]))

;;;
;;;    Can't inherit?!?!
;;;    
(defmethod print-method Counter [counter writer]
  (.write writer (format "%s %d/%d" (.getSimpleName (class counter)) (index counter) (modulus counter))))

(deftype CyclicCounter [clicks limit]
  Counter
  (index [this] @clicks)
  (modulus [this] limit)
  (advance [this] (advance this 1))
  (advance [this n]
;      (reset! clicks (mod (+ @clicks n) limit)))
    (swap! clicks #(mod (+ % n) limit)))
  (set [this n]
    (reset! clicks (mod n limit)))
  (reset [this] (set this 0)))

(defn make-counter [n]
  (if (< n 1)
    (throw (IllegalArgumentException. "Modulus must be at least 1."))
    (CyclicCounter. (atom 0) n)))

(defmethod print-method CyclicCounter [counter writer]
  (.write writer (format "%s %d/%d" (.getSimpleName (class counter)) (index counter) (modulus counter))))

(declare make-persistent-counter)

;(defrecord PersistentCyclicCounter [clicks limit]
(deftype PersistentCyclicCounter [clicks limit]
  Counter
  (index [this] clicks)
  (modulus [this] limit)
  (advance [this] (advance this 1))
  (advance [this n]
    (make-persistent-counter (+ clicks n) limit))
  (set [this n]
    (make-persistent-counter n limit))
  (reset [this] (set this 0)))

;;;
;;;    No way to prevent illegal state by calling constructor directly?
;;;    (PersistentCyclicCounter. 19 0)
;;;    
(defn make-persistent-counter
  ([n] (make-persistent-counter 0 n))
  ([i n]
   (if (< n 1)
     (throw (IllegalArgumentException. "Modulus must be at least 1."))
     (PersistentCyclicCounter. (mod i n) n))))

;;;
;;;    Not recognized!?!?! (With `defrecord`!!)
;;;    
(defmethod print-method PersistentCyclicCounter [counter writer]
  (.write writer (format "%s %d/%d" (.getSimpleName (class counter)) (index counter) (modulus counter))))

;;;
;;;    Ted Cushman
;;;    
(defn counter [limit]
  (let [a (atom (cycle (range limit)))]
    #(first (swap! a next))))

(let [c! (counter 10)] (repeatedly 22 c!))

;;;
;;;    Expose race condition with `reset!`
;;;    
(let [c (make-counter 1000000)] (doall (pmap (fn [_] (advance c)) (range 500))) (index c))
