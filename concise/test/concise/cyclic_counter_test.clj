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
  (is (zero? (cc/index (cc/make-counter 8)))
      "New counter index should be zero.")
  (let [n 10]
    (is (== n (cc/modulus (cc/make-counter n)))
        (format "Modulus of counter should be %d." n)))
  (is (thrown-with-msg? IllegalArgumentException
                        #"Modulus must be at least 1."
                        (cc/make-counter 0))
      "Can't create counter with modulus of 0."))

(deftest test-advance
  (let [c (cc/make-counter 10)]
    (cc/advance c)
    (is (== 1 (cc/index c))
        "Index should be 1 after advancing once."))
  (let [n 10
        c (cc/make-counter n)]
    (cc/advance c n)
    (is (zero? (cc/index c))
        (format "Index should be 0 after advancing %d times." n)))
  (let [n 10
        c (cc/make-counter n)]
    (cc/advance c -2)
    (is (== (- n 2) (cc/index c))
        (format "Index should be %d after advancing -2 times." (- n 2)))))

(deftest test-set
  (let [c (cc/make-counter 10)]
    (cc/advance c)
    (cc/set c 0)
    (is (zero? (cc/index c))
        "Index should be 0 after setting."))
  (let [c (cc/make-counter 10)]
    (cc/advance c 2)
    (cc/set c 0)
    (is (zero? (cc/index c))
        "Index should be 0 after setting."))
  (let [n 10
        c (cc/make-counter n)]
    (cc/set c -4)
    (is (== (- n 4) (cc/index c))
        (format "Index should be %d after setting." (- n 4))))
  (let [n 10
        m 6
        c (cc/make-counter n)]
    (cc/advance c)
    (cc/set c m)
    (is (== (mod m n) (cc/index c))
        (format "Index should be %d after setting." (mod m n))))
  (let [n 10
        m 16
        c (cc/make-counter n)]
    (cc/set c m)
    (is (== (mod m n) (cc/index c))
        (format "Index should be %d after setting." (mod m n)))))

(deftest test-reset
  (let [c (cc/make-counter 10)]
    (cc/advance c)
    (cc/reset c)
    (is (zero? (cc/index c))))
  (let [n 10
        c (cc/make-counter n)]
    (cc/set c (dec n))
    (cc/reset c)
    (is (zero? (cc/index c)))))

(deftest test-rollover
  (let [n 10
        c (cc/make-counter n)]
    (dotimes [_ n]
      (cc/advance c))
    (is (zero? (cc/index c))
        (format "Counter should roll over after advancing %d times." n))))
