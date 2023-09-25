(ns invoice-spec
  (:require
    [clojure.spec.alpha :as s]))

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