(ns functional-programming.cet-analyser
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

; Declared so can be easily referred back to from any function
(declare load-menu)
(def fields [:year :day :january :february :march :april :may :june :july :august :september :october :november :december])
(def void-temp -999)

; Attempt to load the latest CET daily legacy data from 1772 to date into memory
(def cet-data-map-row-seq
  (try
    (let [raw-data (slurp "https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetdl1772on.dat")]
      (re-seq #".{1,70}" raw-data))
    (catch Exception e
      nil)))

(def test-map [{:year "1772" :day "1" :january "32" :february "15"}])


;; Prints the menu to console with a nicely formatted title for this program.
(defn render-menu []
  (println (str
            ; Simple Ascii art for a nice title for this section
            "____ ____ ___    ____ _  _ ____ _    _   _ ____ ____ ____    ___ ____ ____ _    
|    |___  |     |__| |\\ | |__| |     \\_/  [__  |___ |__/     |  |  | |  | |    
|___ |___  |     |  | | \\| |  | |___   |   ___] |___ |  \\     |  |__| |__| |___"

            ; The CET Analyser Tool main menu with options
            "\n\n\nPlease select an option: \n\n"
            "0) Back to Main Menu\n"
            "1) Warmest & Coldest days for each month\n"
            "2) Warmest & Coldest years\n"
            "2) Mean temperature for each month with greatest variation\n\n")))

;; Removes any additional spacing from a given row and splits into sequence at single spaces
(defn parse-row
  [row]
  (str/split (str/trim (str/replace row #" {2,}" " ")) #" "))

;; Maps a given post-parsed row to the fields {:year :day :january...} and their respective values
(defn map-record [row]
  (apply hash-map (interleave fields (parse-row row))))

;; Gets the warmest day of all time for given calendar month
;; params:
;; - month | keyword - relating to a field in the CET data-set mapped record.
;; returns: string (date of warmest day along with temperature)
(defn get-warmest-day-by-month
  [month]
  (loop [data cet-data-map-row-seq ; CET data in seq of rows
         current-max (map-record (first cet-data-map-row-seq)) ; keep track of current min
         index 1] ; keep track of current index iterating
    (if (= index (count cet-data-map-row-seq)) ; Base Case
      current-max ; Return current max
      (if (str/blank? (first data)) ; If the row is blank just skip & recur
        (recur (rest data) current-max (inc index))
        (let [row-temp (Integer/parseInt (month (map-record (first data)))) ; Get the current row temp
              highest-temp (Integer/parseInt (month current-max))] ; Get the highest temp currently
          (if (= row-temp void-temp) ; If -999 then it's a N/A value so skip & recur
            (recur (rest data) current-max (inc index))
            (if (> row-temp highest-temp) ; If the current row temp is greater than the current tracked highest
              (recur (rest data) (map-record (first data)) (inc index)) ; Replace the current tracked highest with current row
              (recur (rest data) current-max (inc index))))))))) ; otherwise recur with rest of seq

;; Gets the coldest day of all time for given calendar month
;; params:
;; - month | keyword - relating to a field in the CET data-set mapped record.
;; returns: string (date of coldest day along with temperature)
(defn get-coldest-day-by-month
  [month] ; current month
  (loop [data cet-data-map-row-seq ; CET data in seq of rows
         current-min (map-record (first cet-data-map-row-seq)) ; keep track of current min
         index 1] ; keep track of current index iterating
    (if (= index (count cet-data-map-row-seq)) ; Base Case
      current-min ; Return current min
      (if (str/blank? (first data)) ; If the row is blank just skip & recur
        (recur (rest data) current-min (inc index))
        (let [row-temp (Integer/parseInt (month (map-record (first data)))) ; Get the current row temp
              lowest-temp (Integer/parseInt (month current-min))] ; Get the lowest temp currently
          (if (= row-temp void-temp) ; If -999 then it's a N/A value so skip & recur
            (recur (rest data) current-min (inc index))
            (if (< row-temp lowest-temp) ; If the current row temp is less than the current tracked lowest
              (recur (rest data) (map-record (first data)) (inc index)) ; Replace the current tracked lowest with current row
              (recur (rest data) current-min (inc index))))))))) ; otherwise recur with rest of seq

(defn format-month-name
  [month-name-as-str]
  (str/upper-case (subs month-name-as-str 1 4)))

;; Displays to console the warmest / coldest days
;; for each given month throughout the whole of 1772 to present
(defn display-warmest-coldest-days-by-month
  []
  (println "\nCalculating min & max temperatures for each month since 1772.\nThis may take a minute... Please wait :)\n")
  (println "MONTH \t\t Coldest \t\t Warmest")
  ;; Recursive loop for printing results to console
  (loop [index 2] ; Starts from 2 because of 2-13 indexes being months in fields (account for 0-based indexing)
    (if (= index (count fields)) ; Base case
      (println "\nPress enter to return to the menu")
      (let [current-month (get fields index)
            warmest (get-warmest-day-by-month current-month)
            coldest (get-coldest-day-by-month current-month)]
        (print (format-month-name (str current-month))) ; Formats the months name to display in nicer table format
        (print "\t\t")
        (print (str (:day coldest) "/" (dec index) "/" (:year coldest))) ; date format
        (print " [")
        (print (float (/ (Integer/parseInt (current-month coldest)) 10))) ; temp for current month / 10
        (print " C]")
        (print "\t")
        (print (str (:day warmest) "/" (dec index) "/" (:year warmest))) ; date format
        (print " [")
        (print (float (/ (Integer/parseInt (current-month warmest)) 10))) ; temp for current month / 10
        (print " C]")
        (println)
        (recur (inc index)))))
  (read-line)
  (load-menu))

(defn avg-row
  [row]
  (loop [index 2
         total 0]
    (if (= index (count fields))
      (/ (double (/ total 12)) 10)
      (let [current-temp (Integer/parseInt (get row index))]
        (if (= void-temp current-temp)
          (recur (inc index) total)
          (recur (inc index) (+ total current-temp)))))))

(defn avg-across-31-days
  [year-long-sequence]
  (loop [index 0
         total 0]
    (if (= index 31)
      (double (/ total 31))
      (recur (inc index) (+ total (avg-row (parse-row (get year-long-sequence index))))))))

(defn get-warmest-year
  [year-min year-max]
  (loop
   [current-year year-min
    highest-temp-year year-min
    highest-temp 1
    seq-tracker 0]
    (if (> current-year year-max)
      {:year highest-temp-year :mean-temp highest-temp}
      (let [current-year-total-temp (avg-across-31-days (subvec (into [] (remove str/blank? cet-data-map-row-seq)) (* seq-tracker 31) (* (inc seq-tracker) 31)))]
        (if (> current-year-total-temp highest-temp)
          (recur (inc current-year) current-year current-year-total-temp (inc seq-tracker))
          (recur (inc current-year) highest-temp-year highest-temp (inc seq-tracker)))))))

(defn get-coldest-year
  [year-min year-max]
  (loop
   [current-year year-min
    lowest-temp-year year-min
    lowest-temp 100
    seq-tracker 0]
    (if (> current-year year-max)
      {:year lowest-temp-year :mean-temp lowest-temp}
      (let [current-year-total-temp (avg-across-31-days (subvec (into [] (remove str/blank? cet-data-map-row-seq)) (* seq-tracker 31) (* (inc seq-tracker) 31)))]
        (if (< current-year-total-temp lowest-temp)
          (recur (inc current-year) current-year current-year-total-temp (inc seq-tracker))
          (recur (inc current-year) lowest-temp-year lowest-temp (inc seq-tracker)))))))

(defn display-warmest-coldest-years
  []
  (print "The Record Mean Warmest year from 1772 to 2022 is: ")
  (let [warmest-result (get-warmest-year 1772 2022)]
    (print (:year warmest-result))
    (print " with an average of ")
    (print (:mean-temp warmest-result))
    (print " C across the year.")
    (println)
    (println "Because 2022's data is not fully populated yet, the result excluding this year is: ")
    (print "Record Mean Warmest year from 1772 to 2021: ")
    (let [warmest-result-excluding-2022 (get-warmest-year 1772 2021)]
      (print (:year warmest-result-excluding-2022))
      (print " with an average of ")
      (print (:mean-temp warmest-result-excluding-2022))
      (print " C across the year.")
      (println)))

  (print "\nThe Record Mean Coldest year from 1772 to 2022 is: ")
  (let [coldest-result (get-coldest-year 1772 2022)]
    (print (:year coldest-result))
    (print " with an average of ")
    (print (:mean-temp coldest-result))
    (print " C across the year.")
    (println)
    (println "Because 2022's data is not fully populated yet, the result excluding this year is: ")
    (print "Record Mean Coldest year from 1772 to 2021: ")
    (let [coldest-result-excluding-2022 (get-coldest-year 1772 2021)]
      (print (:year coldest-result-excluding-2022))
      (print " with an average of ")
      (print (:mean-temp coldest-result-excluding-2022))
      (print " C across the year.")
      (println)))
  (println "\nPress enter to return to the menu")
  (read-line)
  (load-menu))

;; The main menu for the CET section
(defn handle-main-menu-choice [user-input]
  (cond
    (= user-input "0") (println "Returning to Main Menu... \n") ; returns to core / main-menu
    (= user-input "1") (display-warmest-coldest-days-by-month)
    (= user-input "2") (display-warmest-coldest-years)
    :else ((println "Please enter a valid choice...")
           (load-menu))))

(defn load-menu []
  (render-menu)
  (let [user-input (read-line)]
    (handle-main-menu-choice user-input)))

;; Error message to display to console when download data-set issue occurs
(defn cet-download-error
  []
  (println (str "There was a problem downloading the CET data-set...\n\n"
                "Make sure you are connected to the internet.\n"
                "Failing that metoffice.gov.uk may be offline.\n\n"
                "To retry downloading please restart the program!\n\n\n"
                "Press enter to continue back to the main menu..."))
  (read-line))

(defn entry
  []
  (if (nil? cet-data-map-row-seq)
    (cet-download-error) ; Data set could not be downloaded on startup...
    (load-menu)))