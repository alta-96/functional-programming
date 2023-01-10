(ns functional-programming.ascii-to-morse
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; Declared so can be easily referred back to from any function
(declare load-menu)

;; A terminating character to represent the end of parsing text
(def terminating-char \@)
;; terminating-char spec
(s/def ::terminating-char char?)
(s/valid? ::terminating-char terminating-char)

;; 3 spaces between each morse character
(def morse-char-spacing "   ")
;; morse-char-spacing spec
(s/def ::morse-char-spacing string?)
(s/valid? ::morse-char-spacing morse-char-spacing)

;; ascii to morse map
(def ascii-morse-map {\A ".-", \B "-...", \C "-.-.", \D "-..", \E ".", \F "..-.", \G "--.", \H "....", \I "..",
                      \J ".---", \K "-.-", \L ".-..", \M "--", \N "-.", \O "---", \P ".--.", \Q "--.-", \R ".-.",
                      \S "...", \T "-" \U "..-", \V "...-" \W ".--", \X "-..-", \Y "-.--", \Z "--..", \0 "-----",
                      \1 ".----", \2 "..---", \3 "...--", \4 "....-", \5 ".....", \6 "-....", \7 "--...",
                      \8 "---..", \9 "----."})
;; ascii-morse-map spec
(s/def ::ascii-morse-map (s/map-of char? string?))
(s/valid? ::ascii-morse-map {\A ".-", \B "-...", \C "-.-."})

;; Prints the menu to console with a nicely formatted title for this program.
(defn render-menu []
  (println (str
            ; Simple Ascii art for a nice title for this section
            "____ ____ ____ _ _      /    _  _ ____ ____ ____ ____    ____ ____ _  _ _  _ ____ ____ ___ ____ ____ 
|__| [__  |    | |     /     |\\/| |  | |__/ [__  |___    |    |  | |\\ | |  | |___ |__/  |  |___ |__/ 
|  | ___] |___ | |    /      |  | |__| |  \\ ___] |___    |___ |__| | \\|  \\/  |___ |  \\  |  |___ |  \\"

            ; The ASCII / Morse-code converter main menu with options
            "\n\n\nPlease select an option: \n\n"
            "0) Back to Main Menu\n"
            "1) ASCII to Morse-Code\n"
            "2) Morse-Code to ASCII\n\n")))

;; Recursive function for converting an ascii sequence into morse-code
;; Args:
;; - ascii-seq: A sequence of characters in ASCII format
;; - converted: A sequence of characters in morse-code format
;; Returns: The fully converted morse-code sequence (converted) as string.
(defn convert-to-morse
  [ascii-seq converted]
  ;; Base Case
  ;; If the char to convert = terminating-char (@) return converted (conversion finished)
  (if (= terminating-char (first ascii-seq))
    (str/trim converted)
    (if (= \space (first ascii-seq))
      (convert-to-morse (rest ascii-seq) (str converted " " morse-char-spacing))
      ; Recursively call the function with the rest of the ascii-seq and the converted char, and the character spacing
      (convert-to-morse (rest ascii-seq) (str converted (get ascii-morse-map (first ascii-seq)) morse-char-spacing)))))
;; convert-to-morse spec
(s/def ::ascii-seq (s/coll-of string?))
(s/def ::converted (s/coll-of string?))
(s/fdef convert-to-morse
  :args (s/cat :ascii-seq ::ascii-seq :converted ::converted)
  :ret ::converted)

;; Takes users plain ASCII input and sends to convert-to-morse to recursively convert each character
(defn ascii-to-morse
  []
  (println "\nPlease enter your ASCII formatted phrase: ")
  (let [ascii-input (str (read-line) terminating-char)]
    (println (convert-to-morse (seq (str/upper-case ascii-input)) ""))
    (load-menu)))

;; Recursive function for converting a morse-code sequence into ASCII
;; Args:
;; - morse-seq: A sequence of characters in morse-code format
;; - converted: A sequence of characters in ASCII format
;; Returns: The fully converted ASCII sequence (converted) as string.
(defn convert-to-ascii
  [morse-seq converted]
  ;; Base Case
  ;; If the str to convert = terminating-char (@) return converted (conversion finished)
  (if (= (str terminating-char) (first morse-seq))
    (str/trim converted)
    (if (empty? (first morse-seq)) ; If its emtpy, it's a gap between words, not characters, so just add a space to the converted string.
      (convert-to-ascii (rest morse-seq) (str converted " ")) ; Recall recursively with amended converted string and morse-seq.
      ; Lookup the key for the corresponding current iteration first morse-code value... Then recall recursively.
      (convert-to-ascii (rest morse-seq) (str converted (first (for [[k v] ascii-morse-map :when (= v (str/trim (first morse-seq)))] k)))))))
;; convert-to-ascii spec
(s/def ::morse-seq (s/coll-of string?))
(s/def ::converted (s/coll-of string?))
(s/fdef convert-to-ascii
  :args (s/cat :morse-seq ::morse-seq :converted ::converted)
  :ret ::converted)

;; Takes users Morse-Code input and sends to convert-to-ascii to recursively convert each character
(defn morse-to-ascii
  []
  (println "\nPlease enter your Morse-Code formatted phrase: (3 spaces between letters, 7 between words) ")
  (let [morse-input (str (read-line) morse-char-spacing terminating-char)]
    (println (convert-to-ascii (str/split morse-input (re-pattern morse-char-spacing)) ""))
    (load-menu)))

;; The main menu for the ASCII/Morse-Code section
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