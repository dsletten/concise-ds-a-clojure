;;;;
;;;;
;;;;   One of the nice things about Clojure is that it lets you fix Java
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               yfi_keys_test.clj
;;;;
;;;;   Started:            Sat Apr 20 17:30:21 2024
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

(ns concise.yfi-keys-test
  (:refer-clojure :exclude [+ ==])
  (:require [clojure.test :refer [deftest is]]
            [concise.yfi :refer [+ ==] :as yfi]
            [concise.yfi-keys :as yfik]))

(deftest test-make-yfi
  (is (thrown-with-msg? IllegalArgumentException
                        #"Length components must be non-negative integers."
                        (yfik/make-yfi :yards 1 :feet -2 :inches 3)
      "Can't create YFI with negative values."))
  (is (thrown-with-msg? IllegalArgumentException
                        #"Length components must be non-negative integers."
                        (yfik/make-yfi :yards 1.0 :feet 2.0 :inches 3.0)
      "Can't create YFI with non-integer values.")))

(deftest test-length
  (is (clojure.core/== 0 (yfi/length (yfik/make-yfi))))
  (is (clojure.core/== 1 (yfi/length (yfik/make-yfi :inches 1))))
  (is (clojure.core/== 12 (yfi/length (yfik/make-yfi :feet 1))))
  (is (clojure.core/== 36 (yfi/length (yfik/make-yfi :yards 1))))
  (is (clojure.core/== 49 (yfi/length (yfik/make-yfi :yards 1 :feet 1 :inches 1))))
  (is (clojure.core/== 100 (yfi/length (+ (yfik/make-yfi :inches 49) (yfik/make-yfi :inches 51)))))
  (is (clojure.core/== 100 (yfi/length (+ (yfik/make-yfi  :inches 49) 51))))
  (is (clojure.core/== 100 (yfi/length (+ 49 (yfik/make-yfi :inches 51)))))
  (is (clojure.core/== 100 (yfi/length (+ 49 51)))) )

(deftest test-inches
  (is (clojure.core/== 0 (yfi/inches (yfik/make-yfi))))
  (is (clojure.core/== 1 (yfi/inches (yfik/make-yfi :inches 1))))
  (is (clojure.core/== 0 (yfi/inches (yfik/make-yfi :feet 1))))
  (is (clojure.core/== 0 (yfi/inches (yfik/make-yfi :yards 1))))
  (is (clojure.core/== 1 (yfi/inches (yfik/make-yfi :yards 1 :feet 1 :inches 1))))
  (is (clojure.core/== 0 (yfi/inches (+ (yfik/make-yfi :inches 8) (yfik/make-yfi :inches 4)))) ))

(deftest test-feet
  (is (clojure.core/== 0 (yfi/feet (yfik/make-yfi))))
  (is (clojure.core/== 0 (yfi/feet (yfik/make-yfi :inches 1))))
  (is (clojure.core/== 1 (yfi/feet (yfik/make-yfi :feet 1))))
  (is (clojure.core/== 0 (yfi/feet (yfik/make-yfi :yards 1))))
  (is (clojure.core/== 1 (yfi/feet (yfik/make-yfi :yards 1 :feet 1 :inches 1))))
  (is (clojure.core/== 0 (yfi/feet (+ (yfik/make-yfi :inches 16) (yfik/make-yfi :inches 20)))) ))

(deftest test-yards
  (is (clojure.core/== 0 (yfi/yards (yfik/make-yfi))))
  (is (clojure.core/== 0 (yfi/yards (yfik/make-yfi :inches 1))))
  (is (clojure.core/== 0 (yfi/yards (yfik/make-yfi :feet 1))))
  (is (clojure.core/== 1 (yfi/yards (yfik/make-yfi :yards 1))))
  (is (clojure.core/== 1 (yfi/yards (yfik/make-yfi :yards 1 :feet 1 :inches 1))))
  (is (clojure.core/== 1 (yfi/yards (+ (yfik/make-yfi :inches 12) (yfik/make-yfi :inches 12) (yfik/make-yfi :inches 12)))) ))

;;;
;;;    Succeeds as boolean?!
;;;    
(deftest test-+
  (is (true? (instance? concise.yfi.YFI (+))))
  (is (true? (instance? concise.yfi.YFI (+ 1))))
  (is (== 0 (+)))
  (is (== 1 (+ 0 1)))
  (is (== 1 (+ (yfik/make-yfi) (yfik/make-yfi :inches 1))))
  (let [a 10
        b 20]
    (is (== (+ a b) (+ b a))))
  (let [a 20
        b (yfik/make-yfi :inches 30)]
    (is (== (+ a b) (+ b a))))
  (let [a (yfik/make-yfi :inches 20)
        b (yfik/make-yfi :inches 30)]
    (is (== (+ a b) (+ b a))))
  (let [a (yfik/make-yfi :yards 1 :feet 2 :inches 3)
        b (yfik/make-yfi :yards 4 :feet 5 :inches 6)]
    (is (== (+ a b) (+ b a))))
  (let [a (yfik/make-yfi :inches 20)
        b (yfik/make-yfi :inches 30)
        c (yfik/make-yfi :inches 40)]
    (is (== (+ (+ a b) c) (+ a (+ b c)) (+ a b c))))
  (is (== (reduce clojure.core/+ (range 1 11))
          (apply + (range 1 11))
          (apply + (map #(yfik/make-yfi :inches %) (range 1 11))))))

(deftest test-==
  (is (== 0))
  (is (== 1 1))
  (is (not (== 0 1)))
  (let [a 39
        b (yfik/make-yfi :inches 39)
        c (yfik/make-yfi :yards 1 :inches 3)]
    (is (and (== a b c)
             (== a c b)
             (== b a c)
             (== b c a)
             (== c a b)
             (== c b a))))
  (is (== (yfik/make-yfi :inches 5) (+ 2 3) (+ (yfik/make-yfi :inches 2) (yfik/make-yfi :inches 3))))
  (dotimes [_ 100]
    (let [len (rand-int 200)
          yfi1 (concise.yfi.YFI. len)
          yfi2 (yfik/make-yfi :inches (yfi/inches yfi1)
                              :feet   (yfi/feet yfi1)
                              :yards  (yfi/yards yfi1))]
      (is (== yfi1 yfi2)))))
