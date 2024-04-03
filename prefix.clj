;;;;
;;;;
;;;;   Clojure is great for apps where you need access to the bare meta.
;;;;   -- Jay Fields
;;;;
;;;;   Name:               prefix.clj
;;;;
;;;;   Started:            Thu Jun 17 02:44:05 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Evaluate (unparenthesized) prefix arithmetic expression
;;;;       Juxtapose recursive vs. stack-based implementations.
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

(ns prefix
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:require [containers :as c]))

;; (defn operation [operator]
;;   (case operator
;;     + +
;;     * *
;;     ** expt))

;; (def operation {'+ +
;;                 '* *
;;                 '** expt})

(defn lookup [operator]
  (case operator
   (+ - * /) (resolve operator)
   % mod))
  
(defn evaluate [operator op1 op2]
;(println op1 operator op2)
  ((lookup operator) op1 op2))

(defn read-token [st]
  (if (.hasMoreTokens st)
    (read-string (.nextToken st))
    nil))

(defn eval-prefix [s]
  (let [st (java.util.StringTokenizer. s)
        eval-expression (fn eval-expression []
                          (let [token (read-token st)]
                            (if (nil? token)
                              (throw (IllegalStateException. "Missing argument"))
                              (let [expr (read-string token)]
                                (if (number? expr)
                                  expr
                                  (evaluate expr (eval-expression) (eval-expression)))) )))]
    (let [result (eval-expression)]
      (when (.hasMoreTokens st)
        (throw (IllegalStateException. (.nextToken st))))
      result)))

(defn error [& msgs]
  (throw (IllegalStateException. (apply cl-format nil msgs))))

;;;
;;;    Trying out `if-let`
;;;    
(defn eval-prefix [s]
  (let [st (java.util.StringTokenizer. s)
        eval-expression (fn eval-expression []
                          (if-let [token (read-token st)]
                            (if (number? token)
                              token
                              (evaluate token (eval-expression) (eval-expression)))
                            (error "Missing argument")))]
    (let [result (eval-expression)]
      (when (.hasMoreTokens st)
        (error "Malformed expression. Remaining tokens: ~A" (.nextToken st)))
      result)))

(def ^:constant marker :v)

(defn mark [stack]
  (conj stack marker)) ; WTF?! Not a stack after `cons`...

(defn marked? [stack]
  (and (not (empty? stack)) (= (peek stack) marker)))

;;;
;;;    Stack implementation is far more complicated...
;;;    
(defn stack-eval-prefix [s]
  (let [st (java.util.StringTokenizer. s)]
    (letfn [(process [operand operator-stack operand-stack]
              (if (not (marked? operator-stack))
                (process-left-operand operand operator-stack operand-stack)
                (process-right-operand operand operator-stack operand-stack)))
            (process-left-operand [operand operator-stack operand-stack]
              {:operator-stack (mark operator-stack)
               :operand-stack (conj operand-stack operand)})
            (process-right-operand [operand operator-stack operand-stack]
               (let [new-operator-stack (pop operator-stack)]
                 (when (empty? new-operator-stack)
                   (error "Missing operator"))
                 (process (evaluate (peek new-operator-stack) (peek operand-stack) operand) (pop new-operator-stack) (pop operand-stack))))
            (validate [operator-stack operand-stack]
              (cond (empty? operator-stack) (error "Missing argument") ; Operator stack should consist of only marker.
                    (not (marked? operator-stack)) (error "Illegal state") ; Not strictly necessary? Next clause would detect.
                    (empty? operand-stack) (error "Missing expression") ; Final value should be on operand stack.
                    :else (let [result (peek operand-stack)]
                            (when (not (empty? (pop operand-stack)))
                              (error "Too many arguments"))
                            (when (not (empty? (pop operator-stack)))
                              (error "Missing argument"))
                            result)))]
      (loop [token (read-token st)
             operator-stack '()
             operand-stack '()]
        (if (nil? token)
          (validate operator-stack operand-stack)
          (if (number? token)
            (let [{:keys [operator-stack operand-stack]} (process token operator-stack operand-stack)]
              (recur (read-token st) operator-stack operand-stack))
            (recur (read-token st) (conj operator-stack token) operand-stack)))) )))

;;;
;;;    Using my Stack...
;;;    
(defn mark [stack]
  (c/push stack marker))

(defn marked? [stack]
  (and (not (c/empty? stack)) (= (c/top stack) marker)))

;;;
;;;    Stack implementation is far more complicated...
;;;    
(defn stack-eval-prefix [s]
  (let [st (java.util.StringTokenizer. s)]
    (letfn [(process [operand operator-stack operand-stack]
              (if (not (marked? operator-stack))
                (process-left-operand operand operator-stack operand-stack)
                (process-right-operand operand operator-stack operand-stack)))
            (process-left-operand [operand operator-stack operand-stack]
              {:operator-stack (mark operator-stack)
               :operand-stack (c/push operand-stack operand)})
            (process-right-operand [operand operator-stack operand-stack]
               (let [new-operator-stack (c/pop operator-stack)]
                 (when (c/empty? new-operator-stack)
                   (error "Missing operator"))
                 (process (evaluate (c/top new-operator-stack) (c/top operand-stack) operand) (c/pop new-operator-stack) (c/pop operand-stack))))
            (validate [operator-stack operand-stack]
              (cond (c/empty? operator-stack) (error "Missing argument") ; Operator stack should consist of only marker.
                    (not (marked? operator-stack)) (error "Illegal state") ; Not strictly necessary? Next clause would detect.
                    (c/empty? operand-stack) (error "Missing expression") ; Final value should be on operand stack.
                    :else (let [result (c/top operand-stack)]
                            (when (not (c/empty? (c/pop operand-stack)))
                              (error "Too many arguments"))
                            (when (not (c/empty? (c/pop operator-stack)))
                              (error "Missing argument"))
                            result)))]
      (loop [token (read-token st)
             operator-stack (c/empty-stack)
             operand-stack (c/empty-stack)]
        (if (nil? token)
          (validate operator-stack operand-stack)
          (if (number? token)
            (let [{:keys [operator-stack operand-stack]} (process token operator-stack operand-stack)]
              (recur (read-token st) operator-stack operand-stack))
            (recur (read-token st) (c/push operator-stack token) operand-stack)))) )))

(defn test-prefix [f]
  (is (== (f "9") 9))
  (is (== (f "    9    ") 9))
  (is (== (f "+ 2 3") 5))
  (is (== (f "* 3 -6") -18))
  (is (== (f "* + 4 5 9") 81))
  (is (== (f "* + 2 8 % 7 3") 10))
  (is (== (f "% + * 2 3 5 4") 3))
   ;;
   ;;    The following test is iffy. It makes perfect sense in Lisp,
   ;;    but it is probably not the same result as in Ruby/Java.
   ;;    The result of evaluating the first 3 operators (mod (* 2 5) (/ 6 4)) is:
   ;;    (mod 10 6/4) => 1
   ;;    Since
   ;;    (floor 10 6/4) => 6; 1
   ;;    But in Ruby/Java, 6/4 => 1
   ;;    Equivalently:
   ;;    (mod 10 (truncate 6 4)) => 0
   ;;    Since
   ;;    (truncate 6 4) => 1; 2
   ;;    
  (is (== (f "+ % * 2 5 / 6 4 * 2 3") 7))
  (is (== (f "+ 1 + 2 + 3 4") (f "+ + + 1 2 3 4") 10))
  (is (== (f "- 99 * 7 13") (- 99 (* 7 13)))) ; Fox only handles single-digit numerals!
  (is (apply == (map f '("+ 2 3" "+ + 1 1 3" "+ + / 8 8 1 3" "+ + / * 4 2 8 1 3"))))
  (is (apply == (map f '("+ 2 3" "+ 2 + 2 1" "+ 2 + 2 / 8 8" "+ 2 + 2 / 8 * 4 2")))) )

(deftest test-eval-prefix
  (is (test-prefix eval-prefix)))

(deftest test-stack-eval-prefix
  (is (test-prefix stack-eval-prefix)))

