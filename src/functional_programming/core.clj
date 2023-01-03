(ns functional-programming.core
  (:require [functional-programming.ascii-to-morse :as ascii-to-morse])
  (:require [functional-programming.cet-analyser :as cet-analyser]))

(declare load-main-menu)

(defn render-menu []
  (println (str
            "\n\nPlease select the program you wish to run: \n\n"
            "0) Exit\n"
            "1) Ascii/Morse Code converter\n"
            "2) Central England Temperature record analyser tool\n\n")))

(defn shutdown
  []
  (println "Goodbye...")
  (System/exit 0))

(defn launch-ascii-program
  []
  (println "Launching Ascii/Morse code converter program...\n")
  (ascii-to-morse/entry)
  (load-main-menu))

(defn launch-cet-program
  []
  (println "Launching Central England Temperature record analyser program...\n")
  (cet-analyser/entry)
  (load-main-menu))

(defn handle-main-menu-choice [user-input]
  (cond
    (= user-input "0") (shutdown)
    (= user-input "1") (launch-ascii-program)
    (= user-input "2") (launch-cet-program)
    :else ((println "Please enter a valid choice...")
           (load-main-menu))))

(defn load-main-menu []
  (render-menu)
  (let [user-input (read-line)]
    (handle-main-menu-choice user-input)))

(defn -main
  []
  (load-main-menu))