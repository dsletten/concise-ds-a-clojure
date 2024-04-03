;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               infix.clj
;;;;
;;;;   Started:            Fri Jun 18 01:45:39 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Evaluate infix arithmetic expression
;;;;       Juxtapose recursive vs. stack-based implementations.
;;;;
;;;;       Simple case requires fully-parenthesized expression.
;;;;       
;;;;       More sophisticated (recursive descent) establishes precedence without parentheses.
;;;;       Parentheses still have highest precedence.
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

(ns infix
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:require [containers :as c]))

(defn lookup [operator]
  (case operator
   (+ - * /) (resolve operator)
   % mod))
  
(defn evaluate [operator op1 op2]
  ((lookup operator) op1 op2))

(defn eval-expression [expr]
  (cond (nil? expr) (throw (IllegalStateException. "Missing argument"))
        (number? expr) expr
        :else (let [[op1 operator op2] expr]
                (evaluate operator (eval-expression op1) (eval-expression op2)))) )
;;;
;;;    This requires the expression to be fully parenthesized!
;;;    
(defn clojure-eval-infix [s]
  (eval-expression (read-string {:eof nil} s)))

(defn read-token [s]
  (case s
    ("(" ")") (symbol s)
    (read-string s)))

;;;
;;;    This feels kludgy...
;;;    The first alternative must grab the minus sign on a negative integer
;;;    rather than letting the second alternative match is as a subtraction operator.
;;;    
(defn tokenize [s]
  (map read-token (re-seq #"\-?\d+|[-+*/%\(\)]" s)))

(defn skip-whitespace [stream]
  (let [n (.read stream)]
    (if (neg? n)
      nil
      (let [ch (char n)]
        (case ch
          \space (recur stream)
          (.unread stream n)))) ))
  
;;;
;;;    Tokenization is trickier with infix, as pre/postfix expressions do not involve ().
;;;    Can't use basic StringTokenizer since it will grab ( as part of numeric token.
;;;    E.g., "(2 + 3)" -> ["(2" "+" "3)"]
;;;    
;;;    Can't use `read` directly either since expression need not be fully parenthesized,
;;;    i.e., not necessarily a legal S-expression.
;;;
;;;    After skipping whitespace, `read-token` reads individual chars (as ints). The single
;;;    chars \( and \) are converted to symbols. Otherwise the token char is put back in the
;;;    stream and `read` instantiates a Clojure object: number or arithmetic symbol.
;;;    
(defn read-token [stream]
  (skip-whitespace stream)
  (let [n (.read stream)]
    (if (neg? n)
      nil
      (let [ch (char n)]
        (case ch
          (\( \)) (symbol (str ch))
          (do (.unread stream n) (read {:eof nil} stream)))) )))

(defn error [& msgs]
  (throw (IllegalStateException. (apply cl-format nil msgs))))

;;;
;;;    Evaluation is simpler with a sequence of tokens rather than a stream of chars since
;;;    we occasionally have to push a token back after misinterpreting it.
;;;    
(defn tokenize [s]
  (let [stream (java.io.PushbackReader. (java.io.StringReader. s))]
    (loop [tokens []]
      (if-let [token (read-token stream)]
        (recur (conj tokens token))
        tokens))))

;;;
;;;    Sedgewick Algorithms 4e pg. 129
;;;    Must be fully parenthesized.
;;;
;;;    Read one token at a time from stream.
;;;    
(defn stack-eval-infix [s]
  (let [stream (java.io.PushbackReader. (java.io.StringReader. s))]
    (loop [token (read-token stream)
           operator-stack '()
           operand-stack '()]
      (if (nil? token)
        (cond (empty? operand-stack) (error "Missing expression")
              :else (cond (and (empty? (pop operand-stack))
                               (empty? operator-stack))
                          (peek operand-stack)
                          :else (error "Malformed expression")))
        (cond (number? token) (recur (read-token stream) operator-stack (conj operand-stack token))
              (symbol? token) (case token
                                #=(symbol "(") (recur (read-token stream) operator-stack operand-stack) ; Ignore...                                
                                #=(symbol ")") (if (empty? operand-stack)
                                                 (error "Missing argument")
                                                 (let [op2 (peek operand-stack)
                                                       operand-stack (pop operand-stack)]
                                                   (if (empty? operand-stack)
                                                     (error "Missing argument")
                                                     (let [op1 (peek operand-stack)
                                                           operand-stack (pop operand-stack)]
                                                       (if (empty? operator-stack)
                                                         (error "Missing operator")
                                                         (let [op (peek operator-stack)
                                                               operator-stack (pop operator-stack)]
                                                           (recur (read-token stream) operator-stack (conj operand-stack (evaluate op op1 op2)))) )))) )
                                (+ - * / %) (recur (read-token stream) (conj operator-stack token) operand-stack)))) )))

;;;
;;;    Tokenize entire expression first.
;;;    
(defn stack-eval-infix [s]
  (let [evaluate-op (fn [operator-stack operand-stack]
                      (if (empty? operand-stack)
                        (error "Missing argument")
                        (let [op2 (peek operand-stack)
                              operand-stack (pop operand-stack)]
                          (if (empty? operand-stack)
                            (error "Missing argument")
                            (let [op1 (peek operand-stack)
                                  operand-stack (pop operand-stack)]
                              (if (empty? operator-stack)
                                (error "Missing operator")
                                (let [op (peek operator-stack)
                                      operator-stack (pop operator-stack)]
                                  {:operator-stack operator-stack :operand-stack (conj operand-stack (evaluate op op1 op2))}) )))) ))]
    (loop [tokens (tokenize s)
           operator-stack '()
           operand-stack '()]
      (if (empty? tokens)
        (cond (empty? operand-stack) (error "Missing expression")
              :else (cond (and (empty? (pop operand-stack))
                               (empty? operator-stack))
                          (peek operand-stack)
                          :else (error "Malformed expression")))
        (let [token (first tokens)]
          (cond (number? token) (recur (rest tokens) operator-stack (conj operand-stack token))
                (symbol? token) (case token
                                  #=(symbol "(") (recur (rest tokens) operator-stack operand-stack) ; Ignore...                                
                                  #=(symbol ")") (let [{:keys [operator-stack operand-stack]} (evaluate-op operator-stack operand-stack)]
                                                   (recur (rest tokens) operator-stack operand-stack))
                                  (+ - * / %) (recur (rest tokens) (conj operator-stack token) operand-stack)))) ))))

;;;
;;;    Mutually recursive.
;;;    No need for map to simulate multiple values. Both nested functions simply keep passing in necessary args to each other.
;;;    
(defn stack-eval-infix [s]
  (letfn [(evaluate-op [tokens operator-stack operand-stack]
            (if (empty? operand-stack)
              (error "Missing argument")
              (let [op2 (peek operand-stack)
                    operand-stack (pop operand-stack)]
                (if (empty? operand-stack)
                  (error "Missing argument")
                  (let [op1 (peek operand-stack)
                        operand-stack (pop operand-stack)]
                    (if (empty? operator-stack)
                      (error "Missing operator")
                      (let [op (peek operator-stack)
                            operator-stack (pop operator-stack)]
                        (eval-expression tokens operator-stack (conj operand-stack (evaluate op op1 op2)))) )))) ))
          (eval-expression [tokens operator-stack operand-stack]
            (if (empty? tokens)
              (cond (empty? operand-stack) (error "Missing expression")
                    :else (cond (and (empty? (pop operand-stack))
                                     (empty? operator-stack))
                                (peek operand-stack)
                                :else (error "Malformed expression")))
              (let [token (first tokens)]
                (cond (number? token) (recur (rest tokens) operator-stack (conj operand-stack token))
                      (symbol? token) (case token
                                        #=(symbol "(") (recur (rest tokens) operator-stack operand-stack) ; Ignore...                                
                                        #=(symbol ")") (evaluate-op (rest tokens) operator-stack operand-stack)
                                        (+ - * / %) (recur (rest tokens) (conj operator-stack token) operand-stack)))) ))]
    (eval-expression (tokenize s)'()'())))

;;;
;;;    My Stack
;;;    
(defn stack-eval-infix [s]
  (letfn [(evaluate-op [tokens operator-stack operand-stack]
            (if (c/empty? operand-stack)
              (error "Missing argument")
              (let [op2 (c/top operand-stack)
                    operand-stack (c/pop operand-stack)]
                (if (c/empty? operand-stack)
                  (error "Missing argument")
                  (let [op1 (c/top operand-stack)
                        operand-stack (c/pop operand-stack)]
                    (if (c/empty? operator-stack)
                      (error "Missing operator")
                      (let [op (c/top operator-stack)
                            operator-stack (c/pop operator-stack)]
                        (eval-expression tokens operator-stack (c/push operand-stack (evaluate op op1 op2)))) )))) ))
          (eval-expression [tokens operator-stack operand-stack]
            (if (empty? tokens)
              (cond (c/empty? operand-stack) (error "Missing expression")
                    :else (cond (and (c/empty? (c/pop operand-stack))
                                     (c/empty? operator-stack))
                                (c/top operand-stack)
                                :else (error "Malformed expression")))
              (let [token (first tokens)]
                (cond (number? token) (recur (rest tokens) operator-stack (c/push operand-stack token))
                      (symbol? token) (case token
                                        #=(symbol "(") (recur (rest tokens) operator-stack operand-stack) ; Ignore...                                
                                        #=(symbol ")") (evaluate-op (rest tokens) operator-stack operand-stack)
                                        (+ - * / %) (recur (rest tokens) (c/push operator-stack token) operand-stack)))) ))]
    (eval-expression (tokenize s) (c/empty-stack) (c/empty-stack))))

(deftest test-stack-eval-infix
  (is (== (stack-eval-infix "9") 9))
  (is (== (stack-eval-infix "    9    ") 9))
  (is (== (stack-eval-infix "9 ((") 9)) ; This should fail...
  (is (== (stack-eval-infix "(10 / 5)") 2))
  (is (== (stack-eval-infix "(10 / -5)") -2))
  (is (== (stack-eval-infix "(9 * 8)") 72))
  (is (== (stack-eval-infix "(2 + 3)") 5))
  (is (== (stack-eval-infix "(3 * -6)") -18))
  (is (== (stack-eval-infix "((4 + 5) * 9)") 81))
  (is (== (stack-eval-infix "((2 + 8) * (7 % 3))") 10))
  (is (== (stack-eval-infix "(((2 * 3) + 5) % 4)") 3))
  (is (== (stack-eval-infix "(((2 * 5) % (6 / 4)) + (2 * 3))") 7))
  (is (== (stack-eval-infix "((1 + 2) + 3)") 6))
  (is (== (stack-eval-infix "(99 - (7 * 13))") (- 99 (* 7 13)))) )

;;;
;;;    Art of Java - recursive descent parser. Expression need not be fully parenthesized.
;;;    
(defn eval-infix [s]
  (letfn [(eval-expression [tokens]
            (cond (empty? tokens) (error "Missing expression")
                  :else (eval-term (first tokens) (rest tokens))))
          (eval-term [token tokens]
            (let [{op1 :value more :tokens} (eval-factor token tokens)]
              (cond (empty? more) {:value op1 :tokens '()}
                    :else (eval-additive op1 (first more) (rest more)))) )
          (eval-additive [op1 operator tokens]
            (case operator
              (+ -) (if (empty? tokens)
                      (error "Missing argument to ~A" operator)
                      (let [{op2 :value more :tokens} (eval-factor (first tokens) (rest tokens))]
                        (if (empty? more)
                          {:value (evaluate operator op1 op2) :tokens '()}
                          (recur (evaluate operator op1 op2) (first more) (rest more)))) )
              {:value op1 :tokens (cons operator tokens)})) ; Push back token we shouldn't have consumed.
          (eval-factor [token tokens]
            (let [{op1 :value more :tokens} (eval-parenthesized token tokens)]
              (cond (empty? more) {:value op1 :tokens '()}
                    :else (eval-multiplicative op1 (first more) (rest more)))) )
          (eval-multiplicative [op1 operator tokens]
            (case operator
              (* / %) (if (empty? tokens)
                        (error "Missing argument to ~A" operator)
                        (let [{op2 :value more :tokens} (eval-parenthesized (first tokens) (rest tokens))]
                          (if (empty? more)
                            {:value (evaluate operator op1 op2) :tokens '()}
                            (recur (evaluate operator op1 op2) (first more) (rest more)))) )
              {:value op1 :tokens (cons operator tokens)})) ; Push back token we shouldn't have consumed.
          (eval-parenthesized [token tokens]
            (case token
              #=(symbol "(") (let [{result :value more :tokens} (eval-expression tokens)] ; Can't handle naked symbol
                  (when (not= (first more) (symbol ")"))                                  ;    ( or ) here!!
                    (error "Missing delimiter"))
                  {:value result :tokens (rest more)})
               {:value (eval-atom token) :tokens tokens}))
          (eval-atom [token]
            (cond (number? token) token
                  :else (error "Malformed atom: ~A" token)))]
    (let [{result :value tokens :tokens} (eval-expression (tokenize s))]
      (when (not (empty? tokens))
        (error "Malformed expression. Remaining tokens: ~A" tokens))
      result)))
  
(deftest test-infix
  (is (== (eval-infix "9") 9))
  (is (== (eval-infix "(9)") 9)) ; Superfluous ()
  (is (== (eval-infix "    9    ") 9))
  (is (== (eval-infix "(10 / 5)") 2))
  (is (== (eval-infix "10 / 5") 2))
  (is (== (eval-infix "(10 / -5)") -2))
  (is (== (eval-infix "10 / -5") -2))
  (is (== (eval-infix "(9 * 8)") 72))
  (is (== (eval-infix "9 * 8") 72))
  (is (== (eval-infix "2 + 3") 5))
  (is (== (eval-infix "3 * -6") -18))
  (is (== (eval-infix "(4 + 5) * 9") 81))
  (is (== (eval-infix "((2 + 8) * (7 % 3))") 10))
  (is (== (eval-infix "(2 + 8) * (7 % 3)") 10))
  (is (== (eval-infix "(((2 * 3) + 5) % 4)") 3))
  (is (== (eval-infix "(2 * 3 + 5) % 4") 3))
  (is (== (eval-infix "(((2 * 5) % (6 / 4)) + (2 * 3))") 7))
  (is (== (eval-infix "(2 * 5 % (6 / 4)) + 2 * 3") 7))
  (is (apply == 10 (map eval-infix '("(1 + (2 + (3 + 4)))" "(((1 + 2) + 3) + 4)" "1 + (2 + (3 + 4))" "((1 + 2) + 3) + 4" "1 + 2 + 3 + 4"))))
  (is (== (eval-infix "(((1 + 2) + 3))") 6)) ; Superfluous ()
  (is (== (eval-infix "99 - 7 * 13") (- 99 (* 7 13))))
  (is (apply == (map eval-infix '("2 + 3" "(1 + 1) + 3" "(8 / 8 + 1) + 3" "4 * 2 / 8 + 1 + 3"))))
  (is (apply == (map eval-infix '("2 + 3" "2 + (2 + 1)" "2 + (2 + 8 / 8)" "2 + (2 + 8 / (4 * 2))")))) )
