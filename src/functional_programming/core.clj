(ns functional-programming.core
  (:require [functional-programming.ascii-to-morse :as ascii-to-morse])
  (:require [functional-programming.cet-analyser :as cet-analyser]))

(declare load-menu)

;; Prints the menu to console with a nicely formatted title.
(defn render-menu []
  (println (str
            ; Simple Ascii art for a nice title for this section
            "_  _ ____ _ _  _    _  _ ____ _  _ _  _ 
|\\/| |__| | |\\ |    |\\/| |___ |\\ | |  | 
|  | |  | | | \\|    |  | |___ | \\| |__|"

            ; The main menu with its options
            "\n\nPlease select the program you wish to run: \n\n"
            "0) Exit\n"
            "1) Ascii/Morse Code converter\n"
            "2) Central England Temperature record analyser tool\n\n")))

;; Exits the program
(defn shutdown
  []
  (println "Goodbye...")
  (System/exit 0))

(defn launch-ascii-program
  []
  (println "Launching Ascii/Morse code converter program...\n")
  (ascii-to-morse/entry)
  (load-menu))

(defn launch-cet-program
  []
  (println "Launching Central England Temperature record analyser program...\n")
  (cet-analyser/entry)
  (load-menu))

(defn handle-menu-choice [user-input]
  (cond
    (= user-input "0") (shutdown)
    (= user-input "1") (launch-ascii-program)
    (= user-input "2") (launch-cet-program)
    :else ((println "Please enter a valid choice...")
           (load-menu))))

(defn load-menu []
  (render-menu)
  (let [user-input (read-line)]
    (handle-menu-choice user-input)))

(defn -main
  []
  (load-menu))