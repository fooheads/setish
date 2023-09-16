(ns fooheads.setish-test
  "Contains the common tests where the test data doesn't differ (much)
  between collection types."
  (:require
    [clojure.test :refer [are deftest]]
    [fooheads.setish :as set]))


(deftest setish-test
  (are [x y] (= x y)
    (set/setish #{1 2})
    #{1 2}

    (set/setish '(1 2 1))
    '(1 2)

    (set/setish [1 2 1])
    [1 2]))


(deftest map-invert-test
  (are [x y] (= x y)
    (set/map-invert {})
    {}

    (set/map-invert {:a "one" :b "two"})
    {"one" :a "two" :b}))


(deftest rename-keys-test
  (are [x y] (= x y)
    (set/rename-keys
      {:a "one" :b "two"}
      {:a :z})
    {:z "one" :b "two"}

    (set/rename-keys
      {:a "one" :b "two"}
      {:a :z :c :y})
    {:z "one" :b "two"}

    (set/rename-keys
      {:a "one" :b "two" :c "three"}
      {:a :b :b :a})
    {:a "two" :b "one" :c "three"}))

