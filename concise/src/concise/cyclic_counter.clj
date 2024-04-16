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
;;;;   Notes: https://github.com/henryw374/time-literals
;;;;
;;;;
;; exercises:
;; Make the persistent counter resettable using Clojure interface for resetting / clearing collections


(ns concise.cyclic-counter
  (:refer-clojure :exclude [set]))

(defprotocol Counter
  (index [this])
  (modulus [this])
  (advance-by [this n])
  (set [this n]))

;;;
;;;    Default implementations. Can't define as part of protocol.
;;;    
(defn advance
  ([c] (advance-by c 1))
  ([c n] (advance-by c n)))

(defn reset [c]
  (set c 0))

;;;
;;;    Can't inherit?!?!
;;;    
(defmethod print-method Counter [counter writer]
  (.write writer (format "%s %d/%d" (.getSimpleName (class counter)) (index counter) (modulus counter))))

(deftype CyclicCounter [clicks limit]
  Counter
  (index [this] @clicks)
  (modulus [this] limit)
  (advance-by [this n]
    (swap! clicks #(mod (+ % n) limit)))
  (set [this n]
    (reset! clicks (mod n limit))))

(defn make-counter [n]
  (if (< n 1)
    (throw (IllegalArgumentException. "Modulus must be at least 1."))
    (CyclicCounter. (atom 0) n)))

(defmethod print-method CyclicCounter [counter writer]
  (.write writer (format "#counter/cc [%d %d]" (index counter) (modulus counter))))

;; (defn read-cyclic-counter [^CharSequence cs]
;;   (prn cs)
;;   (let [[m n] (read-string cs)
;;         c (make-counter n)]
;;     (prn c)
;;     (advance c m)
;;     (prn c)
;;     c))

;; concise.cyclic-counter> #counter/cc "[2 3]"
;; "[2 3]"
;; #counter/cc [0 3]
;; #counter/cc [2 3]
;; "[2 3]"
;; #counter/cc [0 3]
;; #counter/cc [2 3]
;; Syntax error compiling fn* at (concise:localhost:40519(clj)*:1:7846).
;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Atom@4f6b1878

;; (defn read-cyclic-counter [[m n]]
;;   (prn [m n])
;;   (let [c (make-counter n)]
;;     (prn c)
;;     (advance c m)
;;     (prn c)
;; ;    c))
;; 8))

;; concise.cyclic-counter> #counter/cc [2 3]
;; [2 3]
;; #counter/cc [0 3]
;; #counter/cc [2 3]
;; [2 3]
;; #counter/cc [0 3]
;; #counter/cc [2 3]
;; Syntax error compiling fn* at (concise:localhost:40519(clj)*:1:7846).
;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Atom@2230e800

;;;
;;;    Called twice!
;;;    
;; (defn read-cyclic-counter [[m n]]
;;   (prn [m n])
;;   8)


(defn read-cyclic-counter [[m n]]
;(prn [m n])
  `(let [c# (make-counter ~n)]
     (advance c# ~m)
     c#))

(declare make-persistent-counter)

(defrecord PersistentCyclicCounter [clicks limit]
;(deftype PersistentCyclicCounter [clicks limit]
  Counter
  (index [this] clicks)
  (modulus [this] limit)
  (advance-by [this n]
    (make-persistent-counter (+ clicks n) limit))
  (set [this n]
    (make-persistent-counter n limit)))

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
;;;    Args not evaluated
;;;    https://clojure.org/reference/reader#_deftype_defrecord_and_constructor_calls_version_1_3_and_later
;;;    
;     #concise.cyclic_counter.PersistentCyclicCounter[2 8]

(defn read-persistent-cyclic-counter [[m n]]
  `(make-persistent-counter ~m ~n))

(defmethod print-method PersistentCyclicCounter [counter writer]
  (.write writer (format "#counter/pcc [%d %d]" (index counter) (modulus counter))))

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
