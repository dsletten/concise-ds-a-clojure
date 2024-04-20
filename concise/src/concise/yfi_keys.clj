;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               yfi_keys.clj
;;;;
;;;;   Started:            Sat Apr 20 17:08:44 2024
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

(ns concise.yfi-keys
  (:require [concise.yfi :as yfi]))

(defn make-yfi [& {:keys [yards feet inches]
                   :or {yards 0
                        feet 0
                        inches 0}}]
  (yfi/make-yfi yards feet inches))
