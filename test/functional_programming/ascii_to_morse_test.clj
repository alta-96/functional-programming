(ns functional-programming.ascii-to-morse-test
  (:require [clojure.test :refer :all]
            [functional-programming.ascii-to-morse :refer :all]))

(deftest test-convert-to-morse
  (def ascii-string-literal (str "THIS IS A TEST" terminating-char))
  (def morse-code-equivalent "-   ....   ..   ...       ..   ...       .-       -   .   ...   -   ")

  (def empty-input (str "" terminating-char))
  (testing "The conversion of an ASCII string literal to Morse-Code"
    (is (= (convert-to-morse ascii-string-literal "")  morse-code-equivalent))
    (is (empty? (convert-to-morse empty-input "")))))
