;;;;
;;;;
;;;;   Clojure is great for apps where you need access to the bare meta.
;;;;   -- Jay Fields
;;;;
;;;;   Name:               yfi_keys.clj
;;;;
;;;;   Started:            Sat May  4 17:46:39 2024
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
