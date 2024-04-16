;;;;
;;;;
;;;;   To build a brand new language and use lisp syntax on the JVM, you either gotta be a crazy person, or got some really cool ulterior motive. I met Rich and he's not a crazy person.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               persistent_cyclic_counter_test.clj
;;;;
;;;;   Started:            Mon Apr 15 01:21:36 2024
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

(ns persistent-cyclic-counter-test
  (:require [clojure.test :refer [deftest is]]
            [concise.cyclic-counter :as cc]))

(deftest test-make-persistent-counter
  (is (zero? (cc/index (cc/make-persistent-counter 8)))
      "New counter index should be zero.")
  (let [n 10]
    (is (== n (cc/modulus (cc/make-persistent-counter n)))
        (format "Modulus of counter should be %d." n)))
  (is (thrown-with-msg? IllegalArgumentException
                        #"Modulus must be at least 1."
                        (cc/make-persistent-counter 0))
      "Can't create counter with modulus of 0."))

(deftest test-advance
  (is (== 1 (cc/index (cc/advance (cc/make-persistent-counter 10))))
      "Index should be 1 after advancing once.")
  (let [n 10]
    (is (zero? (cc/index (cc/advance (cc/make-persistent-counter n) n)))
        (format "Index should be 0 after advancing %d times." n)))
  (let [n 10]
    (is (== (- n 2) (cc/index (cc/advance (cc/make-persistent-counter n) -2)))
        (format "Index should be %d after advancing -2 times." (- n 2)))))

(deftest test-set
  (is (zero? (cc/index (cc/set (cc/advance (cc/make-persistent-counter 10)) 0)))
      "Index should be 0 after setting.")
  (is (zero? (cc/index (cc/set (cc/advance (cc/make-persistent-counter 10) 2) 0)))
      "Index should be 0 after setting.")
  (let [n 10]
    (is (== (- n 4) (cc/index (cc/set (cc/make-persistent-counter n) -4)))
        (format "Index should be %d after setting." (- n 4))))
  (let [n 10
        m 6]
    (is (== (mod m n) (cc/index (cc/set (cc/advance (cc/make-persistent-counter n)) m)))
        (format "Index should be %d after setting." (mod m n))))
  (let [n 10
        m 16]
    (is (== (mod m n) (cc/index (cc/set (cc/make-persistent-counter n) m)))
        (format "Index should be %d after setting." (mod m n)))))

(deftest test-reset
  (is (zero? (cc/index (cc/reset (cc/advance (cc/make-persistent-counter 10))))))
  (let [n 10]
    (is (zero? (cc/index (cc/reset (cc/set (cc/make-persistent-counter n) (dec n))))))))

(deftest test-rollover
  (let [n 10]
    (loop [i 0
           pcc (cc/make-persistent-counter n)]
      (if (== i n)
        (is (zero? (cc/index pcc))
            (format "Counter should roll over after advancing %d times." n))
        (recur (inc i) (cc/advance pcc))))))
