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

;; Testing the map-record function
;; Assertions include: 
;;  - A valid input is mapped correctly into the expected map and can be indexed by keyword
(deftest test-map-record
  ; Arrange
  (def sample-row " 1772    1   32  -15   18   25   87  128  187  177  105  111   78  112     ")
  (def expected-january-temperature "32")
  (def expected-september-temperature "105")

  ; Act & Assert
  (testing "map-record maps the row as expected and can be indexed properly"
    (is (= (:january (map-record sample-row)) expected-january-temperature))
    (is (= (:september (map-record sample-row)) expected-september-temperature))))

;; Testing the compare-temps-by-month function
;; Assertions include: 
;;  - The correct row is returned with respect to the comparative (i.e :warmest, :coldest)
;;  - A void temp automatically returns the other row (-999) 
(deftest test-compare-temps-by-month
  ; Arrange
  (def sample-row-1  ; January temp is 32
    " 1772    1   32  -15   18   25   87  128  187  177  105  111   78  112     ")
  (def sample-row-2  ; January temp is 2
    " 1772    1   2  -15   18   25   87  128  187  177  105  111   78  112     ")
  (def sample-void-temp-row ; January temp is -999
    " 1772    1   -999  -15   18   25   87  128  187  177  105  111   78  112     ")

  ; Act & Assert
  (testing "The row with the lowest temp is returned when compared by coldest"
    (is (= (compare-temps-by-month :coldest :january sample-row-1 sample-row-2) sample-row-2)))

  (testing "The row with the highest temp is returned when compared by warmest"
    (is (= (compare-temps-by-month :warmest :january sample-row-1 sample-row-2) sample-row-1)))

  (testing "The row that isn't void-temp is returned, regardless of comparative"
    (is (= (compare-temps-by-month :coldest :january sample-row-2 sample-void-temp-row) sample-row-2))))
