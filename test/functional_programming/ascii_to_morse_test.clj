(ns functional-programming.ascii-to-morse-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [functional-programming.ascii-to-morse :refer :all]))

;; Testing the ASCII to Morse-Code recursive function
;; Assertions include: 
;;  - A regular ascii phrase to morse-code conversion
;;  - An empty input conversion
(deftest test-convert-to-morse
  ; Arrange
  (def ascii-string-literal (str "THIS IS A TEST" terminating-char))
  (def morse-code-equivalent "-   ....   ..   ...       ..   ...       .-       -   .   ...   -")
  (def empty-input (str "" terminating-char))

  ; Act & Assert
  (testing "The conversion of an ASCII string literal to Morse-Code"
    (is (= (convert-to-morse ascii-string-literal "")  morse-code-equivalent))
    (is (empty? (convert-to-morse empty-input "")))))

;; Testing the Morse-Code to ASCII recursive function
;; Assertions include: 
;;  - A regular morse-code phrase to ascii conversion
;;  - An empty input conversion
(deftest test-convert-to-ascii
  ; Arrange
  (def morse-string-literal (str "-   ....   ..   ...       ..   ...       .-       -   .   ...   -"  morse-char-spacing terminating-char))
  (def ascii-code-equivalent "THIS IS A TEST")
  (def empty-input (str ""  morse-char-spacing terminating-char))

  ; Act & Assert
  (testing "The conversion of a Morse-Code string to ASCII string literal"
    (is (= (convert-to-ascii (str/split morse-string-literal (re-pattern morse-char-spacing)) "")  ascii-code-equivalent))
    (is (empty? (convert-to-ascii (str/split empty-input (re-pattern morse-char-spacing)) "")))))