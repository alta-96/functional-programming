(ns functional-programming.cet-analyser
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; Declared so can be easily referred back to from any function
(declare load-menu)

;; GLobals
(def fields [:year :day :january :february :march :april :may :june :july :august :september :october :november :december])
(s/def ::fields (s/coll-of keyword?))
(s/valid? ::fields fields)

(def start-year 1772)
(s/def ::start-year int?)
(s/valid? ::start-year start-year)

(def end-year 2022)
(s/def ::end-year int?)
(s/valid? ::end-year end-year)

(def void-temp -999)
(s/def ::void-temp int?)
(s/valid? ::void-temp void-temp)

; Attempt to load the latest CET daily legacy data from 1772 to date into memory
(def cet-data-map-row-seq
  (try
    (let [raw-data (slurp "https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetdl1772on.dat")]
      (re-seq #".{1,70}" raw-data))
    (catch Exception e
      nil)))

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
            "3) Mean temperature for each month with greatest & smallest variation\n\n")))

;; Removes any additional spacing from a given row and splits into sequence at single spaces
(defn parse-row
  [row]
  (str/split (str/trim (str/replace row #" {2,}" " ")) #" "))
; parse-row spec
(s/def ::row string?)
(s/valid? ::row "test")
(s/def ::formatted-row (s/coll-of string?))
(s/valid? ::formatted-row (parse-row "    this    is    a  test  "))
(s/fdef parse-row
  :args (s/cat :row ::row)
  :ret ::formatted-row)

;; Maps a given post-parsed row to the fields {:year :day :january...} and their respective values
(defn map-record [row]
  (apply hash-map (interleave fields (parse-row row))))
; map-record spec
(s/def ::row string?)
(s/def ::record map?)
(s/fdef map-record
  :args (s/cat :row ::row)
  :ret ::record)

;; Args: - A comparative keyword (:coldest or :warmest)
;;       - The specified month to compare,
;;       - pre-formatted rows (map-record)
;; Returns: Either the coldest or the hottest row depending on
;; specified comparative.
(defn compare-temps-by-month
  [comparative month row-1 row-2]
  (let [row-1-temp (Integer/parseInt (month (map-record row-1)))
        row-2-temp (Integer/parseInt (month (map-record row-2)))]
    (if (= comparative :coldest)
      (if (= row-1-temp void-temp)
        row-2
        (if (= row-2-temp void-temp)
          row-1
          (if (= row-1-temp (min row-1-temp row-2-temp))
            row-1
            row-2)))
      (if (= row-1-temp (max row-1-temp row-2-temp))
        row-1
        row-2))))
; compare-temps-by-month spec
(s/def ::comparative keyword?)
(s/def ::month keyword?)
(s/def ::row string?)
(s/fdef compare-temps-by-month
  :args (s/cat :comparative ::comparative :month ::month :row-1 ::row :row-2 ::row)
  :ret ::row)


;; Args: - A comparative keyword (:coldest or :warmest)
;; Returns: A map of the warmest/coldest day for every
;;          month, over the entire CET dataset.
;;          {:month {:day, :year, :temp}}
;;          For each of the 12 months, where :temp is either
;;          the coldest or warmest, depending on the comparative provided.
(defn get-extremes-by-comparative
  [comparative]
  (loop [data (into [] (remove str/blank? cet-data-map-row-seq))
         jan (first data) feb (first data) mar (first data)
         apr (first data) may (first data) jun (first data)
         jul (first data) aug (first data) sep (first data)
         oct (first data) nov (first data) dec (first data)]
    (if (empty? data) ; Base case
      {:january {:day (:day (map-record jan))
                 :year (:year (map-record jan))
                 :temp (format "%.2f" (/ (double (Integer/parseInt (:january (map-record jan)))) 10))}
       :february {:day (:day (map-record feb))
                  :year (:year (map-record feb))
                  :temp (format "%.2f" (/ (double (Integer/parseInt (:february (map-record feb)))) 10))}
       :march {:day (:day (map-record mar))
               :year (:year (map-record mar))
               :temp (format "%.2f" (/ (double (Integer/parseInt (:march (map-record mar)))) 10))}
       :april {:day (:day (map-record apr))
               :year (:year (map-record apr))
               :temp (format "%.2f" (/ (double (Integer/parseInt (:april (map-record apr)))) 10))}
       :may {:day (:day (map-record may))
             :year (:year (map-record  may))
             :temp (format "%.2f" (/ (double (Integer/parseInt (:may (map-record may)))) 10))}
       :june {:day (:day (map-record jun))
              :year (:year (map-record jun))
              :temp (format "%.2f" (/ (double (Integer/parseInt (:june (map-record jun)))) 10))}
       :july {:day (:day (map-record jul))
              :year (:year (map-record jul))
              :temp (format "%.2f" (/ (double (Integer/parseInt (:july (map-record jul)))) 10))}
       :august {:day (:day (map-record aug))
                :year (:year (map-record aug))
                :temp (format "%.2f" (/ (double (Integer/parseInt (:august (map-record aug)))) 10))}
       :september {:day (:day (map-record sep))
                   :year (:year (map-record sep))
                   :temp (format "%.2f" (/ (double (Integer/parseInt (:september (map-record sep)))) 10))}
       :october {:day (:day (map-record oct))
                 :year (:year (map-record  oct))
                 :temp (format "%.2f" (/ (double (Integer/parseInt (:october (map-record oct)))) 10))}
       :november {:day (:day (map-record nov))
                  :year (:year (map-record nov))
                  :temp (format "%.2f" (/ (double (Integer/parseInt (:november (map-record nov)))) 10))}
       :december {:day (:day (map-record dec))
                  :year (:year (map-record dec))
                  :temp (format "%.2f" (/ (double (Integer/parseInt (:december (map-record dec)))) 10))}}
      (recur (rest data)
             (compare-temps-by-month comparative :january (first data) jan)
             (compare-temps-by-month comparative :february (first data) feb)
             (compare-temps-by-month comparative :march (first data) mar)
             (compare-temps-by-month comparative :april (first data) apr)
             (compare-temps-by-month comparative :may (first data) may)
             (compare-temps-by-month comparative :june (first data) jun)
             (compare-temps-by-month comparative :july (first data) jul)
             (compare-temps-by-month comparative :august (first data) aug)
             (compare-temps-by-month comparative :september (first data) sep)
             (compare-temps-by-month comparative :october (first data) oct)
             (compare-temps-by-month comparative :november (first data) nov)
             (compare-temps-by-month comparative :december (first data) dec)))))
; get-extremes-by-comparative spec
(s/def ::comparative keyword?)
(s/def ::extremes-map map?)
(s/fdef compare-temps-by-month
  :args (s/cat :comparative ::comparative)
  :ret ::extremes-map)

;; Formats month name into short version.
;; January becomes JAN, February becomes FEB, etc...
(defn format-month-name
  [month-name-as-str]
  (str/upper-case (subs month-name-as-str 1 4)))
; format-month-name spec
(s/def ::month-name-as-str string?)
(s/def ::formatted-month string?)
(s/fdef format-month-name
  :args (s/cat :month-name-as-str ::month-name-as-str)
  :ret ::formatted-month)

;; Returns a year long sequence for a given year
;; year 1 = 1772, year 2 = 1773... year 250 = 2022
(defn get-year-long-seq
  [year]
  (subvec (into [] (remove str/blank? cet-data-map-row-seq)) (* year 31) (* (inc year) 31)))
; get-year-long-seq spec
(s/def ::year integer?)
(s/def ::year-long-subvec vector?)
(s/fdef get-year-long-seq
  :args (s/cat :year ::year)
  :ret ::year-long-subvec)

;; Displays to console the warmest / coldest days
;; for each given month throughout the whole of 1772 to present
(defn display-warmest-coldest-days-by-month
  []
  (println "MONTH \t\t Coldest \t\t Warmest")
  (let [warmest-set (get-extremes-by-comparative :warmest)
        coldest-set (get-extremes-by-comparative :coldest)]
    ;; Recursive loop for printing results to console
    (loop [index 2] ; Starts from 2 because of 2-13 indexes being months in fields (account for 0-based indexing)
      (if (= index (count fields)) ; Base case
        (println "\nPress enter to return to the menu")
        (let [current-month (get fields index)]
          (print (format-month-name (str current-month))) ; Formats the months name to display in nicer table format
          (print "\t\t ")
          (print (str (:day (current-month  coldest-set)) "/" (dec index) "/" (:year (current-month coldest-set)))) ; date format
          (print " [")
          (print (:temp (current-month coldest-set)))
          (print "]")
          (print "\t ")
          (print (str (:day (current-month warmest-set)) "/" (dec index) "/" (:year (current-month warmest-set)))) ; date format
          (print " [")
          (print (:temp (current-month warmest-set))) ; temp for current month / 10
          (print "]")
          (println)
          (recur (inc index))))))
  (read-line)
  (load-menu))

;; Args: - A pre-formatted (mapped-record) row.
;; Returns: The average temp for that given row.
(defn avg-row
  [row]
  (loop [index 2
         total 0]
    (if (= index (count fields)) ; Base case
      (/ (double (/ total 12)) 10)
      (let [current-temp (Integer/parseInt (get row index))]
        (if (= void-temp current-temp)
          (recur (inc index) total)
          (recur (inc index) (+ total current-temp)))))))
; avg-row spec
(s/def ::row map?)
(s/def ::avg-temp-for-row int?)
(s/fdef avg-row
  :args (s/cat :row ::row)
  :ret ::avg-temp-for-row)

;; Args: - A year long sequence
;; Returns: The average temp across the 31 days from averages of each row. (the avg for year)
(defn avg-across-31-days
  [year-long-sequence]
  (loop [index 0
         total 0]
    (if (= index 31) ; Base case
      (double (/ total 31))
      (recur (inc index) (+ total (avg-row (parse-row (get year-long-sequence index))))))))
; avg-across-31-days spec
(s/def ::year-long-sequence vector?)
(s/def ::avg-temp-for-year int?)
(s/fdef avg-across-31-days
  :args (s/cat :year-long-sequence ::year-long-sequence)
  :ret ::avg-temp-for-year)

;; Args: - year-min - the starting year
;;       - year-max - the ending year
;; Returns: The warmest year between the provided year-min and year-max
(defn get-warmest-year
  [year-min year-max]
  (loop
   [current-year year-min
    highest-temp-year year-min
    highest-temp 1
    seq-tracker 0]
    (if (> current-year year-max) ; Base case
      {:year highest-temp-year :mean-temp (format "%.2f" highest-temp)}
      (let [current-year-total-temp (avg-across-31-days (get-year-long-seq seq-tracker))]
        (if (> current-year-total-temp highest-temp)
          (recur (inc current-year) current-year current-year-total-temp (inc seq-tracker))
          (recur (inc current-year) highest-temp-year highest-temp (inc seq-tracker)))))))
; get-warmest-year spec
(s/def ::year int?)
(s/def ::warmest-year-info map?)
(s/fdef get-warmest-year
  :args (s/cat :year-min ::year :year-max ::year)
  :ret ::warmest-year-info)

;; Args: - year-min - the starting year
;;       - year-max - the ending year
;; Returns: The coldest year between the provided year-min and year-max
(defn get-coldest-year
  [year-min year-max]
  (loop
   [current-year year-min
    lowest-temp-year year-min
    lowest-temp 100
    seq-tracker 0]
    (if (> current-year year-max)
      {:year lowest-temp-year :mean-temp (format "%.2f" lowest-temp)}
      (let [current-year-total-temp (avg-across-31-days (get-year-long-seq seq-tracker))]
        (if (< current-year-total-temp lowest-temp)
          (recur (inc current-year) current-year current-year-total-temp (inc seq-tracker))
          (recur (inc current-year) lowest-temp-year lowest-temp (inc seq-tracker)))))))
; get-coldest-year spec
(s/def ::year int?)
(s/def ::coldest-year-info map?)
(s/fdef get-warmest-year
  :args (s/cat :year-min ::year :year-max ::year)
  :ret ::coldest-year-info)

;; Prints the warmest & coldest years to console
(defn display-warmest-coldest-years
  []
  (print "The Record Mean Warmest year from 1772 to 2022 is: ")
  (let [warmest-result (get-warmest-year start-year end-year)]
    (print (:year warmest-result))
    (print " with an average of ")
    (print (:mean-temp warmest-result))
    (print " C across the year.")
    (println)
    (println "Because 2022's data is not fully populated yet, the result excluding this year is: ")
    (print "Record Mean Warmest year from 1772 to 2021: ")
    (let [warmest-result-excluding-2022 (get-warmest-year start-year (dec end-year))]
      (print (:year warmest-result-excluding-2022))
      (print " with an average of ")
      (print (:mean-temp warmest-result-excluding-2022))
      (print " C across the year.")
      (println)))

  (print "\nThe Record Mean Coldest year from 1772 to 2022 is: ")
  (let [coldest-result (get-coldest-year start-year end-year)]
    (print (:year coldest-result))
    (print " with an average of ")
    (print (:mean-temp coldest-result))
    (print " C across the year.")
    (println)
    (println "Because 2022's data is not fully populated yet, the result excluding this year is: ")
    (print "Record Mean Coldest year from 1772 to 2021: ")
    (let [coldest-result-excluding-2022 (get-coldest-year start-year (dec end-year))]
      (print (:year coldest-result-excluding-2022))
      (print " with an average of ")
      (print (:mean-temp coldest-result-excluding-2022))
      (print " C across the year.")
      (println)))
  (println "\nPress enter to return to the menu")
  (read-line)
  (load-menu))

;; Gets the specified month average for a given year-long-seq (year block)
;; Args: - month - keyword of month
;;       - year-long-seq - vector comprising of 31 rows (a year block)
;; Returns: The year of that sequence and the average for the specified month
(defn avg-across-31-days-by-month
  [month year-long-seq]
  (loop
   [remaining-mls year-long-seq
    running-total-for-month 0]
    (if (empty? remaining-mls)
      {:year (:year (map-record (first year-long-seq)))
       :month-avg (/ (double (/ running-total-for-month 31)) 10)}
      (let [mapped-day (map-record (first remaining-mls))
            temp (Integer/parseInt (month mapped-day))]
        (if (= temp void-temp) ; account for -999 results
          (recur (rest remaining-mls) (+ running-total-for-month 0))
          (recur (rest remaining-mls) (+ running-total-for-month temp)))))))
; avg-across-31-days-by-month spec
(s/def ::month keyword?)
(s/def ::year-long-seq vector?)
(s/def ::avg-for-given-month-across-year map?)
(s/fdef avg-across-31-days-by-month
  :args (s/cat :month ::month :year-long-seq ::year-long-seq)
  :ret ::avg-for-given-month-across-year)

;; Args: - current-smallest - int of current smallest temp
;;       - contending-new-smallest - int of a temp to compare to current-smallest
;; Returns: The smallest temp of the two
(defn get-new-month-smallest
  [current-smallest
   contending-new-smallest]
  (if (= (:month-avg contending-new-smallest) (min (:month-avg current-smallest) (:month-avg contending-new-smallest)))
    contending-new-smallest
    current-smallest))
; get-new-month-smallest spec
(s/def ::temp int?)
(s/fdef get-new-month-smallest
  :args (s/cat :current-smallest ::temp :comtending-new-smallest ::temp)
  :ret ::temp)

;; Args: - current-largest - int of current largest temp
;;       - contending-new-largest - int of a temp to compare to current-largest
;; Returns: The largest temp of the two
(defn get-new-month-largest
  [current-largest
   contending-new-largest]
  (if (= (:month-avg contending-new-largest) (max (:month-avg current-largest) (:month-avg contending-new-largest)))
    contending-new-largest
    current-largest))
; get-new-month-largest spec
(s/def ::temp int?)
(s/fdef get-new-month-largest
  :args (s/cat :current-largest ::temp :comtending-new-largest ::temp)
  :ret ::temp)

;; Iterates through the CET dataset by year...
;; Calculates the average across all years for a given month,
;; Also finds the smallest average and greatest average variance year for the same month.
;; Args: - month - keyword of month to find avgs & variance avgs for
;; Returns: Map of data consisting of:
;; mean-temp - the mean temp across the entire CET dataset for the given month
;; smallest-variation - the smallest variation of mean-temp average
;; smallest-instance - the year at which this smallest variation is from
;; largest-variation - the largest variation of mean-temp average
;; largest-instance - the year at which this largest variation is from
(defn get-mean-temp-by-month
  [month]
  (loop
   [seq-tracker 0
    running-month-avg 0
    smallest-avg 100
    smallest-avg-instance 1772
    largest-avg 0
    largest-avg-instance 1772]
    (if (= seq-tracker (- end-year start-year)) ; Base case
      {:mean-temp (format "%.2f" (double (/ running-month-avg (- end-year start-year))))
       :smallest-variation-temp (format "%.2f" smallest-avg)
       :smallest-instance smallest-avg-instance
       :largest-variation-temp (format "%.2f" largest-avg)
       :largest-instance largest-avg-instance}
      (let [average (avg-across-31-days-by-month month (get-year-long-seq seq-tracker))]
        (let [new-smallest (get-new-month-smallest {:month-avg smallest-avg :year smallest-avg-instance} average)]
          (let [new-largest (get-new-month-largest {:month-avg largest-avg :year largest-avg-instance} average)]
            (recur
             (inc seq-tracker)
             (+ running-month-avg (:month-avg average))
             (:month-avg new-smallest)
             (:year new-smallest)
             (:month-avg new-largest)
             (:year new-largest))))))))
; get-new-month-largest spec
(s/def ::month keyword?)
(s/def ::avg-with-variance-for-month map?)
(s/fdef get-mean-temp-by-month
  :args (s/cat :month ::month)
  :ret ::avg-with-variance-for-month)

;; Displays the mean along with its smallest & largest variance for each month to console.
(defn display-mean-per-month-with-variation
  []
  (println "\nCalculating mean temperatures for each month since 1772.\nThis may take a minute... Please wait :)\n")
  (println "MONTH \t\t Mean \t\t Smallest Mean Variation \t Greatest Mean Variation ")
  ;; Recursive loop for printing results to console
  (loop [index 2] ; Starts from 2 because of 2-13 indexes being months in fields (account for 0-based indexing)
    (if (= index (count fields)) ; Base case
      (println "\nPress enter to return to the menu")
      (let [current-month (get fields index)
            mean (get-mean-temp-by-month current-month)]
        (print (format-month-name (str current-month))) ; Formats the months name to display in nicer table format
        (print "\t\t ")
        (print (:mean-temp mean))
        (print "\t\t ")
        (print (:smallest-variation-temp mean))
        (print " @ ")
        (print (:smallest-instance mean))
        (print "\t\t\t ")
        (print (:largest-variation-temp mean))
        (print " @ ")
        (print (:largest-instance mean))
        (println)
        (recur (inc index)))))
  (read-line)
  (load-menu))

;; The main menu for the CET section
(defn handle-main-menu-choice [user-input]
  (if (nil? user-input)
    (println "Please enter a valid choice.")
    (case user-input
      "0" (println "Returning to Main Menu... \n")
      "1" (display-warmest-coldest-days-by-month)
      "2" (display-warmest-coldest-years)
      "3" (display-mean-per-month-with-variation)
      :else (println "Please enter a valid choice...")
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