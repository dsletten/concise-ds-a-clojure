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

;; Tagged literal for reader
;; Clojure collections have `clear` distinct from java.util.Collectio
;; exercises:
;; make a custom class printable and readable as a tagged literal value
;; Make the persistent counter resettable using Clojure interface for resetting / clearing collections
;; The Counter protocol has more methods that it needs, which methods can be removed from the protocol and implemented with delegating functions

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

;; (defmethod print-method CyclicCounter [counter writer]
;;   (.write writer (format "%s %d/%d" (.getSimpleName (class counter)) (index counter) (modulus counter))))

(defmethod print-method CyclicCounter [counter writer]
  (.write writer (format "#cc [%d %d]" (index counter) (modulus counter))))

;; *data-readers*
;; {counter/cyclic-counter #'concise.cyclic-counter/read-cyclic-counter,
;;  dbg #'cider.nrepl.middleware.debug/debug-reader,
;;  break #'cider.nrepl.middleware.debug/breakpoint-reader,
;;  light #'cider.nrepl.middleware.enlighten/light-reader}

;; (defn read-cyclic-counter [^CharSequence cs]
;; (prn cs)
;;   (prn (re-matches #"\[(\d+)\s+(\d+)\]" cs))
;; ;;  cs)
;; ;;  (println [m n])
;; ;;  m)
;;   ;; (let [[m n] (read-string cs)
;;   (let [[_ m n] (re-matches #"\[(\d+)\s+(\d+)\]" cs)
;;         c (make-counter (Integer/parseInt n))]
;; (prn c)
;;     (advance c (Integer/parseInt m))
;; (prn c)
;; c))
;; ;(CyclicCounter. (atom (Integer/parseInt m)) (Integer/parseInt n))))

(defn read-cyclic-counter [[m n]]
  `(let [c# (make-counter ~n)]
     (advance c# ~m)
     c#))

(declare make-persistent-counter)

(defrecord PersistentCyclicCounter [clicks limit]
;(deftype PersistentCyclicCounter [clicks limit]
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

;; (defn make-persistent-counter [n]
;;   (if (< n 1)
;;     (throw (IllegalArgumentException. "Modulus must be at least 1."))
;;     (PersistentCyclicCounter. 0 n)))

(defn read-persistent-cyclic-counter [[m n]]
  `(make-persistent-counter ~m ~n))

;;;
;;;    Not recognized!?!?! (With `defrecord`!!)  REPL!!!
;;;    
;; (defmethod print-method PersistentCyclicCounter [counter writer]
;;   (.write writer (format "%s %d/%d" (.getSimpleName (class counter)) (index counter) (modulus counter))))

(defmethod print-method PersistentCyclicCounter [counter writer]
  (.write writer (format "#pcc [%d %d]" (index counter) (modulus counter))))

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


;{:cc #cc [1 5] :pcc #pcc [2 9]}
