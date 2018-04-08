(ns a3.db
  (:require [clojure.string :refer [split split-lines join]]))

; Split the file in a vector of lines, each containing a vector of words
; param{String} file: Path to the file to read and format
; return{Vector[Vector[String]]}: A vector of lines each separated in vectors of data elements
(defn format-file [file]
  ; Split the file by lines, then split each line by components
  (vec (map (fn [s] (split s #"\|")) (split-lines (slurp file)))))

; Get database map from file
; param{String} file: Path to the file to load as a database
; return{Map[Vector[String]]}: A map using the ID as a key and the data vector as a value
(defn to-database [file]
  ; This collapses the vector of line into a single map
  ; I love reduce
  (reduce (fn [db line] (assoc db (keyword (first line)) (vec (rest line)))) {} (reverse (format-file file))))

; Load all three database files
(def customers-db (to-database "cust.txt"))
(def products-db  (to-database "prod.txt"))
(def sales-db     (to-database "sales.txt"))


; Format a database entry into a string
; param{MapEntry} entry: Map entry to format
; return{String}: The string representation of the entry
(defn format-entry [entry]
  (str (name (entry 0)) ":[" (join ", " (entry 1)) "]"))

; Format a sales database entry into a string
; param{MapEntry} entry: Map entry to format
; return{String}: The string representation of the entry
(defn format-sale [entry]
  (let [data (entry 1)]
    (str (name (entry 0)) ":[" ((customers-db (keyword (data 0))) 0) ", " ((products-db (keyword (data 1))) 0) ", " (data 2) "]")))

; Gets the string representation of a database
; param{Map[Vector[String]]} db: Database to get the string representation from
; param{Function} formatted: MapEntry formatting function
; return{String}: The string representation of the database
(defn get-string [db, formatter]
  (join "\n" (map formatter (seq db))))

; Get database string representations
(def customers-text (get-string customers-db format-entry))
(def products-text  (get-string products-db format-entry))
(def sales-text     (get-string sales-db format-sale))


; Creates a name to ID conversion map from databases
; param{Map[Vector[String]]} db: Database to create the conversion map from
; return{Map[Keyword]}: The resulting conversion map
(defn name-to-id [db]
  ; Did I say that I love reduce?
  (reduce (fn [result entry] (assoc result ((entry 1) 0) (name (entry 0)))) {} (reverse (seq db))))

; Get the customers and products conversion maps
(def customers-to-id (name-to-id customers-db))
(def products-to-id (name-to-id products-db))

; Gets the total sales to a given customer
; param{String} name: Name of the customer to get the sales for
; return{String}: String report of the total sales for the customer, or an informative message if he could not be found
(defn get-customer-total [name]
  ; Check if the customer exists
  (if (contains? customers-to-id name)
    ; Format message
    (str name " final bill: "
      (format "%.2f"
        ; Add all sales for customer
        (apply +
          (for [sale sales-db
                :let [data (sale 1)]
                :when (= (data 0) (customers-to-id name))]
            (* ; Multiply price by amount purchased
              (read-string ((products-db (keyword (data 1))) 1)) ; Product price
              (read-string (data 2))))))                         ; Amount purchased
      "$")
  "The customer could not be found in the database"))


; Gets the total purchased amount of a given product
; param{String} name: Name of the product to get the amount for
; return{String}: String report of all the sold products, or an informative message if it could not be found
(defn get-product-count [name]
  ; Check if the product exists
  (if (contains? products-to-id name)
    ; Format message
    (str name " sold: "
      ; Add all sold products
      (apply +
        (for [sale sales-db
              :let [data (sale 1)]
              :when (= (data 1) (products-to-id name))]
          (read-string (data 2)))))
    "The product could not be found in the database"))
