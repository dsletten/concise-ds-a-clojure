;;;;
;;;;
;;;;   I think of Clojure as kind of the greatest hits of the last 20 or 30 years of computer science. It's like that mix tape from the Guardians of the Galaxy, only in software.
;;;;   -- Russ Olsen
;;;;
;;;;   Name:               yfi_test.clj
;;;;
;;;;   Started:            Sat May  4 17:53:05 2024
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

(ns concise.yfi-test
  (:refer-clojure :exclude [+ ==])
  (:require [clojure.test :refer [deftest is]]
            [concise.yfi :refer [+ ==] :as yfi]))

(deftest test-make-yfi
  (is (thrown-with-msg? IllegalArgumentException
                        #"Length components must be non-negative integers."
                        (yfi/make-yfi 1 -2 3)
      "Can't create YFI with negative values."))
  (is (thrown-with-msg? IllegalArgumentException
                        #"Length components must be non-negative integers."
                        (yfi/make-yfi 1.0 2.0 3.0)
      "Can't create YFI with non-integer values.")))

(deftest test-length
  (is (clojure.core/== 0 (yfi/length (yfi/make-yfi))))
  (is (clojure.core/== 1 (yfi/length (yfi/make-yfi 1))))
  (is (clojure.core/== 12 (yfi/length (yfi/make-yfi 1 0))))
  (is (clojure.core/== 36 (yfi/length (yfi/make-yfi 1 0 0))))
  (is (clojure.core/== 49 (yfi/length (yfi/make-yfi 1 1 1))))
  (is (clojure.core/== 100 (yfi/length (+ (yfi/make-yfi 49) (yfi/make-yfi 51))))))

(deftest test-inches
  (is (clojure.core/== 0 (yfi/inches (yfi/make-yfi))))
  (is (clojure.core/== 1 (yfi/inches (yfi/make-yfi 1))))
  (is (clojure.core/== 0 (yfi/inches (yfi/make-yfi 1 0))))
  (is (clojure.core/== 0 (yfi/inches (yfi/make-yfi 1 0 0))))
  (is (clojure.core/== 1 (yfi/inches (yfi/make-yfi 1 1 1))))
  (is (clojure.core/== 0 (yfi/inches (+ (yfi/make-yfi 8) (yfi/make-yfi 4)))) ))

(deftest test-feet
  (is (clojure.core/== 0 (yfi/feet (yfi/make-yfi))))
  (is (clojure.core/== 0 (yfi/feet (yfi/make-yfi 1))))
  (is (clojure.core/== 1 (yfi/feet (yfi/make-yfi 1 0))))
  (is (clojure.core/== 0 (yfi/feet (yfi/make-yfi 1 0 0))))
  (is (clojure.core/== 1 (yfi/feet (yfi/make-yfi 1 1 1))))
  (is (clojure.core/== 0 (yfi/feet (+ (yfi/make-yfi 16) (yfi/make-yfi 20)))) ))

(deftest test-yards
  (is (clojure.core/== 0 (yfi/yards (yfi/make-yfi))))
  (is (clojure.core/== 0 (yfi/yards (yfi/make-yfi 1))))
  (is (clojure.core/== 0 (yfi/yards (yfi/make-yfi 1 0))))
  (is (clojure.core/== 1 (yfi/yards (yfi/make-yfi 1 0 0))))
  (is (clojure.core/== 1 (yfi/yards (yfi/make-yfi 1 1 1))))
  (is (clojure.core/== 1 (yfi/yards (+ (yfi/make-yfi 12) (yfi/make-yfi 12) (yfi/make-yfi 12)))) ))

(deftest test-+
  (is (true? (instance? concise.yfi.YFI (+))))
  (is (true? (instance? concise.yfi.YFI (+ (yfi/make-yfi 1)))))
  (is (== (yfi/make-yfi) (+)))
  (let [a (yfi/make-yfi 1)]
    (and (is (== a (+ (yfi/make-yfi) a)))
         (is (== a (+ a (yfi/make-yfi))))))
  (let [a (yfi/make-yfi 20)
        b (yfi/make-yfi 30)]
    (is (== (+ a b) (+ b a))))
  (let [a (yfi/make-yfi 1 2 3)
        b (yfi/make-yfi 4 5 6)]
    (is (== (+ a b) (+ b a))))
  (let [a (yfi/make-yfi 20)
        b (yfi/make-yfi 30)
        c (yfi/make-yfi 40)]
    (is (== (+ (+ a b) c) (+ a (+ b c)) (+ a b c))))
  (is (clojure.core/== (reduce clojure.core/+ (range 1 11))
                       (yfi/length (apply + (map yfi/make-yfi (range 1 11)))))))

(deftest test-==
  (is (== (yfi/make-yfi)))
  (is (== (yfi/make-yfi 1) (yfi/make-yfi 1)))
  (is (not (== (+) (yfi/make-yfi 1))))
  (is (== (yfi/make-yfi 5) (+ (yfi/make-yfi 2) (yfi/make-yfi 3))))
  (let [a (+ (yfi/make-yfi 20) (yfi/make-yfi 19))
        b (yfi/make-yfi 39)
        c (yfi/make-yfi 1 0 3)]
    (is (and (== a b c)
             (== a c b)
             (== b a c)
             (== b c a)
             (== c a b)
             (== c b a))))
  (dotimes [_ 100]
    (let [len (rand-int 200)
          yfi1 (concise.yfi.YFI. len)
          yfi2 (yfi/make-yfi (yfi/yards yfi1) (yfi/feet yfi1) (yfi/inches yfi1))]
      (is (== yfi1 yfi2)))))
