;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               postfix.clj
;;;;
;;;;   Started:            Sun Jul  4 17:47:20 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Evaluate (unparenthesized) postfix arithmetic expression
;;;;       Juxtapose recursive vs. stack-based implementations.
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

(ns postfix
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:require [containers :as c]))

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

(defn error [& msgs]
  (throw (IllegalStateException. (apply cl-format nil msgs))))

(defn eval-postfix [s]
  (let [st (java.util.StringTokenizer. s)]
    (letfn [(eval-expression-start []
              (let [token (read-token st)]
                (if (nil? token)
                  (error "Missing argument")
                  (if (number? token)
                    (eval-expression-1 token)
                    (error "Malformed postfix expression.")))) )
            (eval-expression-1 [op1]
              (let [token (read-token st)]
                (if (nil? token)
                  op1
                  (if (number? token)
                    (eval-expression-1 (eval-expression-2 op1 token))
                    (error "Malformed postfix expression.")))) )
            (eval-expression-2 [op1 op2]
              (let [token (read-token st)]
                (if (nil? token)
                  (error "Missing argument")
                  (cond (number? token) (eval-expression-2 op1 (eval-expression-2 op2 token))
                        (symbol? token) (case token
                                          (+ - * / %) (evaluate token op1 op2))
                        :else (error "Malformed postfix expression.")))) )]
      (eval-expression-start))))

;;;
;;;    Trying out `if-let`
;;;    
(defn eval-postfix [s]
  (let [st (java.util.StringTokenizer. s)]
    (letfn [(eval-expression-start []
              (if-let [token (read-token st)]
                (if (number? token)
                  (eval-expression-1 token)
                  (error "Malformed postfix expression."))
                (error "Missing argument")))
            (eval-expression-1 [op1]
              (if-let [token (read-token st)]
                (if (number? token)
                  (eval-expression-1 (eval-expression-2 op1 token))
                  (error "Malformed postfix expression."))
                op1))
            (eval-expression-2 [op1 op2]
              (if-let [token (read-token st)]
                (cond (number? token) (eval-expression-2 op1 (eval-expression-2 op2 token))
                      (symbol? token) (case token
                                        (+ - * / %) (evaluate token op1 op2))
                      :else (error "Malformed postfix expression."))
                (error "Missing argument")))]
      (eval-expression-start))))

(defn stack-eval-postfix [s]
  (let [st (java.util.StringTokenizer. s)]
    (loop [token (read-token st)
           stack '()]
      (if (nil? token)
        (cond (empty? stack) (error "Missing expression")
              (not (empty? (pop stack))) (error "Too many arguments")
              :else (peek stack))
        (cond (number? token) (recur (read-token st) (conj stack token))
              (symbol? token) (case token
                               (+ - * / %) (if (empty? stack)
                                             (error "Missing argument")
                                             (let [right (peek stack)
                                                   stack (pop stack)]
                                               (if (empty? stack)
                                                 (error "Missing argument")
                                                 (let [left (peek stack)
                                                       stack (pop stack)]
                                                   (recur (read-token st) (conj stack (evaluate token left right)))) ))))
              :else (error "Malformed postfix expression.")))) ))

;;;
;;;    My Stack
;;;    
(defn stack-eval-postfix [s]
  (let [st (java.util.StringTokenizer. s)]
    (loop [token (read-token st)
           stack (c/empty-stack)]
      (if (nil? token)
        (cond (c/empty? stack) (error "Missing expression")
              (not (c/empty? (c/pop stack))) (error "Too many arguments")
              :else (c/top stack))
        (cond (number? token) (recur (read-token st) (c/push stack token))
              (symbol? token) (case token
                               (+ - * / %) (if (c/empty? stack)
                                             (error "Missing argument")
                                             (let [right (c/top stack)
                                                   stack (c/pop stack)]
                                               (if (c/empty? stack)
                                                 (error "Missing argument")
                                                 (let [left (c/top stack)
                                                       stack (c/pop stack)]
                                                   (recur (read-token st) (c/push stack (evaluate token left right)))) ))))
              :else (error "Malformed postfix expression.")))) ))

(defn test-postfix [f]
  (is (== (f "9") 9))
  (is (== (f "    9    ") 9))
  (is (== (f "2 3 -") -1))
  (is (== (f "3 2 -") 1)) ; Sheesh, Nathan!
  (is (== (f "2 3 +") 5))
  (is (== (f "3 -6 *") -18))
  (is (== (f "4 5 + 9 *") 81))
  (is (== (f "2 8 + 7 3 % *") 10))
  (is (== (f "2 3 * 5 + 4 %") 3))
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
  (is (== (f "2 5 * 6 4 / % 2 3 * +") 7))
  (is (== (f "1 2 3 4 + + +") (f "1 2 + 3 + 4 +") 10))
  (is (== (f "99 7 13 * -") (- 99 (* 7 13)))) ; Fox only handles single-digit numerals!
  (is (apply == (map f '("2 3 +" "1 1 + 3 +" "8 8 / 1 + 3 +" "4 2 * 8 / 1 + 3 +"))))
  (is (apply == (map f '("2 3 +" "2 2 1 + +" "2 2 8 8 / + +" "2 2 8 4 2 * / + +")))) )

(deftest test-eval-postfix
  (is (test-postfix eval-postfix)))

(deftest test-stack-eval-postfix
  (is (test-postfix stack-eval-postfix)))
