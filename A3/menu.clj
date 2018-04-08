(ns a3.menu
  (:require [a3.db :as db]))

; Menu prompt message
(def prompt
" ====== Sales Menu ======

1. Display customer table
2. Display product table
3. Display sales table
4. Total sales for customer
5. Total count for product
6. Exit
")

; Asks for a menu option and repeats until a valid option is entered
; return{Integer}: An integer between 1 and 6 inclusively which represents the option entered by the user
(defn get-input []
  ; Prompt
  (println "Please enter a number between 1 and 6:")
  ; Get user input
  (let [n (read-string (read-line))]
    ; Check if input is valid and in bounds
    (if (and (number? n) (and (>= n 1) (<= n 6)))
      n   ; Return value
      (do ; Ask again
        (println "Entered number is invalid!")
        (recur)))))

; Gets a string prompt from the user
; param{String} message: Prompt message to the user
; return{String}: The string value entered by the user
(defn get-name-input [message]
  ; Print prompt
  (println message)
  ; Get user answer
  (read-line))

; Database menu loop, runs forever until the user exits
(defn customer-loop []
  ; Print menu prompt
  (println prompt)
  ; Get user option
  (let [n (get-input)]
    ; Check if must quit (option 6)
    (if (= n 6)
      (println "Exiting the database menu...\nGoodbye!")
      (do ; Else Do appropriate action
        (case n
          1 (println db/customers-text)
          2 (println db/products-text)
          3 (println db/sales-text)
          4 (println (db/get-customer-total (get-name-input "Please enter the name of the customer: ")))
          5 (println (db/get-product-count (get-name-input "Please enter the name of the product:  "))))
        (println) ; Print empty spacing line then start again
        (recur)))))

; Main entry point of the application
(defn -main []
  ; Run menu loop
	(customer-loop))
