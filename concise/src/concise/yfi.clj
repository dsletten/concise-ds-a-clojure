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
  (:refer-clojure :exclude [+ =]))

(defrecord YFI [length]
  (length [this] (:length this)))

(defn- feet->inches [feet]
  (* feet 12))

(defn- yards->inches [yards]
  (* yards 36))

(defn make-yfi
  ([o]
   ;; (case (class o)
   ;;   concise.yfi.YFI o
   ;;   (YFI. o))))
   (if (instance? YFI o)
     o
     (YFI. o)))
  ([feet inches]
   (YFI. (clojure.core/+ (feet->inches feet) inches)))
  ([yards feet inches]
   (YFI. (clojure.core/+ (yards->inches yards) (feet->inches feet) inches))))

(defmethod print-method YFI [yfi writer]
  (.write writer (format "%s yards: %d feet: %d inches: %d"
                         (.getSimpleName (class yfi))
                         (yards yfi)
                         (feet yfi)
                         (inches yfi))))
                         

;;  (+ 3 4)

;; (cond (p f g h i j q)
;;       (r s)
;;       (t x))

;; (cond p q
;;       r s
;;       t x)

;; (condp p v
;;   x1 y1
;;   x2 y2)

;; (condp p v
;;   (x1 y1)
;;   (x2 :>> y2)
;;   (n))

;; (cond (zerop pos)
;;       (error "Can't start with -")

;;       (= 1 pos)
;;       (let ((start (char char-range 0))
;; 	    (end   (char char-range 2)))
;; 	(if (char< start end)
;; 	  (concatenate 'string
;; 		       (coerce (make-range start end) 'string)
;; 		       (expand (subseq char-range 3)))
;; 	  (error "Inverted range")))

;;       :else (concatenate 'string (subseq char-range 0 (1- pos))
;; 			 (expand (subseq char-range (1- pos)))))
