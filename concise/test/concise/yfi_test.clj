;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               yfi_test.clj
;;;;
;;;;   Started:            Wed Jul 19 02:25:48 2023
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
  (is (clojure.core/== 100 (yfi/length (+ (yfi/make-yfi 49) (yfi/make-yfi 51)))))
  (is (clojure.core/== 100 (yfi/length (+ (yfi/make-yfi 49) 51))))
  (is (clojure.core/== 100 (yfi/length (+ 49 (yfi/make-yfi 51)))))
  (is (clojure.core/== 100 (yfi/length (+ 49 51)))) )

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

;; (is (instance? concise.yfi.YFI (+)) nil)
;; (clojure.test/try-expr nil (instance? concise.yfi.YFI (+)))

;; (try
;;   (let [klass__9664__auto__ concise.yfi.YFI
;;         object__9665__auto__ (+)]
;;     (let [result__9666__auto__ (instance?
;;                                  klass__9664__auto__
;;                                  object__9665__auto__)]
;;       (if result__9666__auto__
;;         (clojure.test/do-report
;;           {:type :pass,
;;            :expected '(instance? concise.yfi.YFI (+)),
;;            :actual (class object__9665__auto__),
;;            :message nil})
;;         (clojure.test/do-report
;;           {:type :fail,
;;            :expected '(instance? concise.yfi.YFI (+)),
;;            :actual (class object__9665__auto__),
;;            :message nil}))
;;       result__9666__auto__))
;;   (catch
;;     java.lang.Throwable
;;     t__9676__auto__
;;     (clojure.test/do-report
;;       {:type :error,
;;        :expected '(instance? concise.yfi.YFI (+)),
;;        :actual t__9676__auto__,
;;        :message nil})))

;(clojure.test/test-var #'test-+)

;;;
;;;    Succeeds in isolation
;;;    
(deftest test-+
  (is (instance? concise.yfi.YFI (+))))

;;;
;;;    Fails if anything follows?!?!
;;;
(deftest test-+
  (is (instance? concise.yfi.YFI (+)))
  (is (== 0 (+))))

;;;
;;;    Succeeds as boolean?!
;;;    
(deftest test-+
  (is (true? (instance? concise.yfi.YFI (+))))
  (is (true? (instance? concise.yfi.YFI (+ 1))))
  (is (== 0 (+)))
  (is (== 1 (+ 0 1)))
  (is (== 1 (+ (yfi/make-yfi) (yfi/make-yfi 1))))
  (let [a 10
        b 20]
    (is (== (+ a b) (+ b a))))
  (let [a 20
        b (yfi/make-yfi 30)]
    (is (== (+ a b) (+ b a))))
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
  (is (== (reduce clojure.core/+ (range 1 11))
          (apply + (range 1 11))
          (apply + (map yfi/make-yfi (range 1 11))))))

(deftest test-==
  (is (== 0))
  (is (== 1 1))
  (is (not (== 0 1)))
  (let [a 39
        b (yfi/make-yfi 39)
        c (yfi/make-yfi 1 0 3)]
    (is (and (== a b c)
             (== a c b)
             (== b a c)
             (== b c a)
             (== c a b)
             (== c b a))))
  (is (== (yfi/make-yfi 5) (+ 2 3) (+ (yfi/make-yfi 2) (yfi/make-yfi 3)))) )
