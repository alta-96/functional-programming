(ns functional-programming.cet-analyser)

; Declared so can be easily referred back to from any function
(declare load-menu)

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
            "1) ASCII to Morse-Code\n"
            "2) Morse-Code to ASCII\n\n")))

(defn ascii-to-morse
  [])

(defn morse-to-ascii
  [])

;; The main menu for the CET section
(defn handle-main-menu-choice [user-input]
  (cond
    (= user-input "0") (println "Returning to Main Menu... \n") ; returns to core / main-menu
    (= user-input "1") (ascii-to-morse)
    (= user-input "2") (morse-to-ascii)
    :else ((println "Please enter a valid choice...")
           (load-menu))))

(defn load-menu []
  (render-menu)
  (let [user-input (read-line)]
    (handle-main-menu-choice user-input)))

(defn entry
  []
  (load-menu))