(ns functional-programming.cet-analyser
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

; Declared so can be easily referred back to from any function
(declare load-menu)

(def fields [:year :day :january :february :march :april :may :june :july :august :september :october :november :december])

; Attempt to load the latest CET daily legacy data from 1772 to date into memory
(def cet-data-map-row-seq
  (try
    (let [raw-data (slurp "https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetdl1772on.dat")]
      (re-seq #".{1,70}" raw-data))
    (catch Exception e
      nil)))

(def test-map [{:year "1772" :day "1" :january "32" :february "15"}])

(type test-map)

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
  (month (map-record (first cet-data-map-row-seq))))

;; Gets the coldest day of all time for given calendar month
;; params:
;; - month | keyword - relating to a field in the CET data-set mapped record.
;; returns: string (date of coldest day along with temperature)
(defn get-coldest-day-by-month
  [month]
  (month (map-record (first cet-data-map-row-seq))))

;; Displays to console the warmest / coldest days
;; for each given month throughout the whole of 1772 to present
(defn display-warmest-coldest-days
  []
  (println "\t\t Warmest \t Coldest")
  (println (str "January:  \t")
           (get-warmest-day-by-month :january) "\t\t" (get-coldest-day-by-month :january))
  (println (str "February:\t")
           (get-warmest-day-by-month :february) "\t\t" (get-coldest-day-by-month :february))
  (println (str "March:\t\t")
           (get-warmest-day-by-month :march) "\t\t" (get-coldest-day-by-month :march))
  (println (str "April:\t\t")
           (get-warmest-day-by-month :april) "\t\t" (get-coldest-day-by-month :april))
  (println (str "May:\t\t")
           (get-warmest-day-by-month :may) "\t\t" (get-coldest-day-by-month :may))
  (println (str "June:\t\t")
           (get-warmest-day-by-month :june) "\t\t" (get-coldest-day-by-month :june))
  (println (str "July:\t\t")
           (get-warmest-day-by-month :july) "\t\t" (get-coldest-day-by-month :july))
  (println (str "August:\t\t")
           (get-warmest-day-by-month :august) "\t\t" (get-coldest-day-by-month :august))
  (println (str "September:\t")
           (get-warmest-day-by-month :september) "\t\t" (get-coldest-day-by-month :september))
  (println (str "October:\t")
           (get-warmest-day-by-month :october) "\t\t" (get-coldest-day-by-month :october))
  (println (str "November:\t")
           (get-warmest-day-by-month :november) "\t\t" (get-coldest-day-by-month :november))
  (println (str "December:\t")
           (get-warmest-day-by-month :december) "\t\t" (get-coldest-day-by-month :december))

  (println "\nPress enter to return to the menu")
  (read-line))

;; The main menu for the CET section
(defn handle-main-menu-choice [user-input]
  (cond
    (= user-input "0") (println "Returning to Main Menu... \n") ; returns to core / main-menu
    (= user-input "1") (display-warmest-coldest-days)
    (= user-input "2") ()
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