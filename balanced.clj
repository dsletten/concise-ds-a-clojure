;;;;
;;;;
;;;;   Clojure is great for apps where you need access to the bare meta.
;;;;   -- Jay Fields
;;;;
;;;;   Name:               balanced.clj
;;;;
;;;;   Started:            Tue Jul 13 21:12:36 2021
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

(ns balanced
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(defn peek-char [stream]
  (let [n (.read stream)]
    (if (neg? n)
      nil
      (let [ch (char n)]
        (.unread stream n)
        ch))))
  
(defn read-char [stream]
  (.read stream))

(defn recursive-balanced-stream [s]
  (let [stream (java.io.PushbackReader. (java.io.StringReader. s))]
    (letfn [(current []
              (peek-char stream))
            (advance []
              (read-char stream))
            (check-sequential []
              (cond (nil? (current)) true
                    (= (current) \[)
                    (do (advance)
                        (cond (nil? (current)) false
                              (check-nested)
                              (cond (nil? (current)) false
                                    (= (current) \])
                                    (do (advance)
                                        (check-sequential))
                                    :else false)
                              :else false))
                    :else false))
            (check-nested []
              (cond (nil? (current)) false ; Incomplete enclosing expression (Only on recursive calls.)
                    (= (current) \[)
                    (do (advance)
                        (cond (nil? (current)) false
                              (check-nested)
                              (cond (nil? (current)) false
                                    (= (current) \])
                                    (do (advance)
                                        (check-nested))
                                    :else false)
                              :else false))
                    (= (current) \]) true ; End of parent enclosing expression
                    :else false))]
      (check-sequential))))

(deftest test-balanced
  (is (recursive-balanced-stream ""))
  (is (not (recursive-balanced-stream "[")))
  (is (not (recursive-balanced-stream "]")))
  (is (recursive-balanced-stream "[]"))
  (is (not (recursive-balanced-stream "[[]")))
  (is (not (recursive-balanced-stream "[]]")))
  (is (not (recursive-balanced-stream "[]]fads")))
  (is (not (recursive-balanced-stream "[]fads")))
  (is (recursive-balanced-stream "[[]]"))
  (is (not (recursive-balanced-stream "[a[]]")))
  (is (not (recursive-balanced-stream "[[]a]")))
  (is (not (recursive-balanced-stream "[[a]]")))
  (is (not (recursive-balanced-stream "[[]]a")))
  (is (recursive-balanced-stream "[[[]]]"))
  (is (recursive-balanced-stream "[][]"))
  (is (recursive-balanced-stream "[[][]]"))
  (is (recursive-balanced-stream "[[]][[]]"))
  (is (not (recursive-balanced-stream "[[]a[]]")))
  (is (recursive-balanced-stream "[[[][]][]]"))
  (is (not (recursive-balanced-stream "[[[]][]][]]")))
  (is (recursive-balanced-stream "[][][[][[]]]"))
  (is (recursive-balanced-stream "[[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]]"))
  (is (not (recursive-balanced-stream "[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]]")))
  (is (not (recursive-balanced-stream "[[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]")))
  (is (not (recursive-balanced-stream "[[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]]"))))
