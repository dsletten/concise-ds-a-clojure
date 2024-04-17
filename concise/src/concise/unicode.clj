;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               unicode.clj
;;;;
;;;;   Started:            Mon Apr 15 22:10:06 2024
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

(ns concise.unicode
  (:require [clojure.pprint :refer [cl-format]]))

(defn print-unicode []
  (loop [i 0]
    (when (< i 1024)
      (cl-format true "~4D ~:@(U+~4,'0X~) ~C~%" i i (char i))
      (recur (inc i)))))



