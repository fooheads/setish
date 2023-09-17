(ns fooheads.setish-test
  "Contains the common tests where the test data doesn't differ (much)
  between collection types."
  (:require
    [clojure.test :refer [are deftest is testing]]
    [fooheads.setish :as set]
    [fooheads.test :refer [thrown-ex-data]]))


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


(deftest index-unique
  (testing "happy case"
    (is (= (set/index-unique
             [{:a 1 :b 1} {:a 1 :b 2}]
             [:b])

           {{:b 1} {:a 1 :b 1}
            {:b 2} {:a 1 :b 2}})))

  (testing "an empty key is allowed as long as it's one and only one value"
    (is (= {{:a 1}   {:a 1 :b 1}
            {:a nil} {:a nil :b 3}
            {}       {:b 2}}
           (set/index-unique
             [{:a 1 :b 1} {:a nil :b 3} {:b 2}]
             [:a]))))

  (testing "empty key should fail"
    (is (= {:msg "Not exactly one value for key {}" :k {}}
           (thrown-ex-data
             [:msg :k]
             (set/index-unique
               [{:a 1 :b 1} {:a 1 :b 2}]
               [:c])))))

  (testing "multiple values for key should fail"
    (is (= {:msg "Not exactly one value for key {:a 1}"
            :k {:a 1}
            :xs [{:a 1 :b 1} {:a 1 :b 2}]}

           (thrown-ex-data
             [:msg :k :xs]
             (set/index-unique
               [{:a 1 :b 1} {:a 1 :b 2}]
               [:a]))))))


