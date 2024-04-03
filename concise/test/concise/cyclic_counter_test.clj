;;;;
;;;;
;;;;   One of the nice things about Clojure is that it lets you fix Java
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               cyclic_counter_test.clj
;;;;
;;;;   Started:            Mon Jul  3 20:25:28 2023
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

(ns concise.cyclic-counter-test
  (:require [clojure.test :refer [deftest is]]
            [concise.cyclic-counter :as cc]))

(deftest test-make-counter
  (is (thrown-with-msg? IllegalArgumentException
                        #"Modulus must be at least 1."
                        (cc/make-counter 0))))

(deftest test-advance
  (let [c (cc/make-counter 10)]
    (cc/advance c)
    (is (= 1 (cc/index c))))
  (let [n 10
        c (cc/make-counter n)]
    (cc/advance c n)
    (is (zero? (cc/index c))))
  (let [n 10
        c (cc/make-counter n)]
    (cc/advance c -2)
    (is (= (- n 2) (cc/index c)))))

(deftest test-counter
  (let [c (cc/make-counter 10)]
    (dotimes [_ 11]
      (cc/advance c))
    (is (= 1 (cc/index c)))))
