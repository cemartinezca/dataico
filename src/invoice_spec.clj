(ns invoice-spec
  (:require
    [clojure.spec.alpha :as s]
    [clojure.data.json :as json] 
    [clojure.string :as str] 
    [clj-time.format :as f]))

(defn not-blank? [value] (-> value clojure.string/blank? not))
(defn non-empty-string? [x] (and (string? x) (not-blank? x)))

(s/def :customer/name non-empty-string?)
(s/def :customer/email non-empty-string?)
(s/def :invoice/customer (s/keys :req [:customer/name
                                       :customer/email]))

(s/def :tax/rate double?)
(s/def :tax/category #{:iva})
(s/def ::tax (s/keys :req [:tax/category
                           :tax/rate]))
(s/def :invoice-item/taxes (s/coll-of ::tax :kind vector? :min-count 1))

(s/def :invoice-item/price double?)
(s/def :invoice-item/quantity double?)
(s/def :invoice-item/sku non-empty-string?)

(s/def ::invoice-item
  (s/keys :req [:invoice-item/price
                :invoice-item/quantity
                :invoice-item/sku
                :invoice-item/taxes]))

(s/def :invoice/issue-date inst?)
(s/def :invoice/items (s/coll-of ::invoice-item :kind vector? :min-count 1))

(s/def ::invoice
  (s/keys :req [:invoice/issue-date
                :invoice/customer
                :invoice/items]))


;; Problem 1 Thread-last Operator ->>

(defn is-valid-item?
  [item]
  (let [taxes (:taxable/taxes item)
        retentions (:retentionable/retentions item)
        has-iva-19? (some #(= (:tax/rate %) 19) taxes)
        has-rete-1? (some #(= (:retention/rate %) 1) retentions)]
    (if (and has-iva-19? has-rete-1?)
      false
      (or has-iva-19? has-rete-1? false))))

(def invoice-data
  (->> (slurp "invoice.edn")
       (clojure.edn/read-string)
       (:invoice/items)
       (filter is-valid-item?)))

(println invoice-data)

;; Problem 2: Core Generating Functions

(defn get-value
  [key value]
  (def time-keys ["issue_date" "payment_date"])
  
  (cond 
    (some #{key} time-keys) (let [source-formatter (f/formatter "dd/MM/yyyy")
                                  dest-formatter (f/formatter "yyyy-MM-dd")
                                  parsed-date (f/parse source-formatter value)
                                  dest-date (f/unparse dest-formatter parsed-date)
                                  result (clojure.instant/read-instant-date dest-date)]
                              result)
    (= key "items") (let [invoice-items (map 
                                         #(reduce (fn [new-map [k v]]
                                                    (if (= k "taxes")
                                                      (assoc new-map (keyword "invoice-item" k) (get-value k v))
                                                      (assoc new-map (keyword "invoice-item" k) v)))
                                                  {}
                                                  %) value)
                          result (into [] invoice-items)]
                      result)
    (= key "taxes") (let [taxes (map #(let [category :iva
                                            rate (get % "tax_rate")]
                                        {(keyword "tax" "category") category
                                         (keyword "tax" "rate") (double rate)}) value)
                          result (into [] taxes)]
                      result)
    (= key "customer") (let [name (get value "company_name")
                             email (get value "email")
                             result {:customer/name name :customer/email email}]
                         result)
    :else  value))

;; (inst? (get-value "payment_date" "12/11/2020"))

(defn organize-map
  ([m] (organize-map {} [] m))
  ([result-map path m]
   (reduce (fn [new-map [key val]]
             (let [new-path (conj path (str/replace key #"_" "-"))
                   key-word (keyword (first new-path) (clojure.string/join "/" (rest new-path)))
                   final-value (get-value key val)]
               (cond
                 (= key "customer") (assoc new-map key-word final-value)
                 (map? val) (organize-map new-map new-path val)
                 :else (assoc new-map key-word final-value))))
           result-map
           m)))

(def invoice-json (-> (slurp "invoice.json")
                      (json/read-str)
                      (organize-map)))

;; (organize-map (json/read-str (slurp "invoice.json")))


(s/explain ::invoice invoice-json)
(s/valid? ::invoice invoice-json)