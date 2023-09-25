(ns invoice-item
  (:require
   [clojure.test :as t]))

(defn- discount-factor [{:invoice-item/keys [discount-rate]
                         :or                {discount-rate 0}}]
  (- 1 (/ discount-rate 100.0)))

(defn subtotal
  [{:invoice-item/keys [precise-quantity precise-price discount-rate]
    :as                item
    :or                {discount-rate 0}}]
  (* precise-price precise-quantity (discount-factor item)))

;; Problem 3: Test Driven Development

;; Defining mock data

(def invoice-data-unit
  {:invoice-item/precise-quantity 1
   :invoice-item/precise-price 5000})

(def invoice-data-items
  {:invoice-item/precise-quantity 5
   :invoice-item/precise-price 5000})

(def invoice-data-discount-0
  {:invoice-item/precise-quantity 10
   :invoice-item/precise-price 5000
   :invoice-item/discount-rate 0})

(def invoice-data-discount-10
  {:invoice-item/precise-quantity 10
   :invoice-item/precise-price 5000
   :invoice-item/discount-rate 10})

(def invoice-data-discount-100
  {:invoice-item/precise-quantity 10
   :invoice-item/precise-price 5000
   :invoice-item/discount-rate 100})

;; Creating test cases

(t/deftest test-unit-price
  (t/testing "Context of the test assertions"
    (t/is (= 5000.0 (subtotal invoice-data-unit)))))

(t/deftest test-many-items
  (t/testing "Context of the test assertions"
    (t/is (= 25000.0 (subtotal invoice-data-items)))))

(t/deftest test-invoice-with-discount-0
  (t/testing "Context of the test assertions"
    (t/is (= 50000.0 (subtotal invoice-data-discount-0)))))

(t/deftest test-invoice-with-discount-10
  (t/testing "Context of the test assertions"
    (t/is (= 45000.0 (subtotal invoice-data-discount-10)))))

(t/deftest test-invoice-with-discount-100
  (t/testing "Context of the test assertions"
    (t/is (= 0.0 (subtotal invoice-data-discount-100)))))

;; Running test cases

(t/run-tests 'invoice-item)
