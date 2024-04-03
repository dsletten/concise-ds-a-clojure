;;;;
;;;;
;;;;   To build a brand new language and use lisp syntax on the JVM, you either gotta be a crazy person, or got some really cool ulterior motive. I met Rich and he's not a crazy person.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               containers.clj
;;;;
;;;;   Started:            Fri Jul 16 00:55:48 2021
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

(ns containers
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(defprotocol Container
  (size [this])
  (empty? [this])
  (clear [this]))

;;;
;;;    There's a name conflict with `clear` above!
;;;    Apparently java.util.Collection.clear()???
;;;
;; (clear *1)
;; Execution error (UnsupportedOperationException) at containers.PersistentQueue/clear (containers.clj:99).
;; null

;;;
;;;    DISPENSER
;;;    
;; (defclass dispenser (container)
;;   ()
;;   (:documentation "A dispenser is a non-traversable container."))

(defprotocol Stack
  (push [this obj])
  (pop [this])
  (top [this]))

;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-STACK...
;;;
(defrecord PersistentStack [top count]
  Container
  (size [this] (:count this))
  (empty? [this] (clojure.core/empty? (:top this)))
  (clear [this] (println this) (PersistentStack. '() 0))
  Stack
  (push [this obj] (PersistentStack. (cons obj (:top this)) (inc (:count this))))
  (pop [this] 
    (if (empty? this)
      (throw (IllegalStateException. "Stack is empty"))
      (PersistentStack. (rest (:top this)) (dec (:count this)))) )
  (top [this]
    (if (empty? this)
      (throw (IllegalStateException. "Stack is empty"))
      (first (:top this)))) )

(defn empty-stack []
  (PersistentStack. '() 0))

(defn push-all
  ([elts] (push-all (empty-stack) elts))
  ([stack elts]
   (reduce #(push %1 %2) stack elts)))
  
(defprotocol Queue
  (enqueue [this obj])
  (dequeue [this])
  (front [this]))

;;;
;;;    `elements`?? To list...
;;;    

;; ;;;    - Invariant: Whenever queue is not empty, front list must be non-empty.
;; ;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-QUEUE...
;; ;;;
(defrecord PersistentQueue [front rear count]
  Container
  (size [this] (:count this))
  (empty? [this] (zero? (:count this)))
  (clear [this] (PersistentQueue. '() '() 0))
  ;; (clear [this] 
  ;;   (loop [q this]
  ;;     (if (empty? q)
  ;;       q
  ;;       (recur (dequeue q)))) )
  Queue
  (enqueue [this obj]
    (if (empty? this)
      (PersistentQueue. (list obj) '() 1)
      (PersistentQueue. (:front this) (cons obj (:rear this)) (inc (:count this)))) )
  (dequeue [this] 
    (cond (empty? this) (throw (IllegalStateException. "Queue is empty"))
          (clojure.core/empty? (rest (:front this))) (PersistentQueue. (reverse (:rear this)) '() (dec (:count this)))
          :else (PersistentQueue. (rest (:front this)) (:rear this) (dec (:count this)))) )
  (front [this]
    (if (empty? this)
      (throw (IllegalStateException. "Queue is empty"))
      (first (:front this)))) )

(defn empty-queue []
  (PersistentQueue. '() '() 0))

;;;
;;;    Enqueue list into PersistentQueue
;;;    
(defn enqueue-all
  ([elts] (enqueue-all (empty-queue) elts))
  ([q elts]
   (reduce #(enqueue %1 %2) q elts)))

;;;
;;;    Map?
;;;    To list?
;;;    
(defn dequeue-all [q]
  (loop [queue q]
    (when (not (empty? queue))
      (do (println (front queue))
          (recur (dequeue queue)))) ))
