(ns functional-programming.cet-analyser-test
  (:require [clojure.test :refer :all]
            [functional-programming.cet-analyser :refer :all]))

;; Testing the parse-row function
;; Assertions include: 
;;  - A valid input is converted into the expected sequence
;;  - An empty input conversion
(deftest test-parse-row
  ; Arrange
  (def sample-input "      this    is    a test  ")
  (def expected-output ["this" "is" "a" "test"])

  ; Act & Assert
  (testing "parse-row appropriately parses the string into a sequence and handles empty input"
    (is (= (parse-row sample-input) expected-output))
    (is (= (parse-row "") [""]))))
