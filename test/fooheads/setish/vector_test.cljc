(ns fooheads.setish.vector-test
  "Tests for the set compatibility of setish. Most of the test code here is
  a clone from the tests for clojure.set in order to ensure compatibility.

  https://github.com/clojure/clojure/blob/c89bf2e9bcfc1bca62e36dee2d78a48f8c38c15c/test/clojure/test_clojure/clojure_set.clj
  "
  (:require
    [clojure.string :as str]
    [clojure.test :refer [are deftest]]
    [fooheads.setish :as set]))


(def compositions
  [{:name "Art of the Fugue" :composer "J. S. Bach"}
   {:name "Musical Offering" :composer "J. S. Bach"}
   {:name "Requiem" :composer "Giuseppe Verdi"}
   {:name "Requiem" :composer "W. A. Mozart"}])


(deftest subset?-test
  (are [sub super] (set/subset? sub super)
    []           []
    []           [1]
    [1]          [1]
    [1 2]        [1 2]
    [1 2]        [1 2 42]
    [false]      [false]
    [nil]        [nil]
    [nil]        [nil false]
    [1 2 nil]    [1 2 nil 4])

  (are [notsub super] (not (set/subset? notsub super))
    [1]          []
    [2]          [1]
    [1 3]        [1]
    [nil]        [false]
    [false]      [nil]
    [false nil]  [nil]
    [1 2 nil]    [1 2]))


(deftest superset?-test
  (are [super sub] (set/superset? super sub)
    []                 []
    [1]                []
    [1]                [1]
    [1 2]              [1 2]
    [1 2 42]           [1 2]
    [false]            [false]
    [nil]              [nil]
    [false nil]        [false]
    [1 2 4 nil false]  [1 2 nil])

  (are [notsuper sub] (not (set/superset? notsuper sub))
    []                 [1]
    [2]                [1]
    [1]                [1 3]
    [nil]              [false]
    [false]            [nil]
    [nil]              [false nil]
    [nil 2 3]          [false nil 2 3]))


(deftest index-test
  (are [x y] (= x y)
    (set/index
      [{:c 2} {:b 1} {:a 1 :b 2}]
      [:b])

    {{:b 1} [{:b 1}]
     {:b 2} [{:a 1 :b 2}],
     {}     [{:c 2}]}))


(deftest difference-test
  (are [x y] (= x y)
    ;; identity
    (set/difference []) []
    (set/difference [1]) [1]
    (set/difference [1 2 3]) [1 2 3]

    ;; 2 sets
    (set/difference [1 2] [1 2]) []
    (set/difference [1 2] [3 4]) [1 2]
    (set/difference [1 2] [1]) [2]
    (set/difference [1 2] [2]) [1]
    (set/difference [1 2 4] [2 3 4 5]) [1]

    ;; 3 sets
    (set/difference [1 2] [2 3] [5 2]) [1]
    (set/difference [1 2 3] [1 3 4] [1 3]) [2]
    (set/difference [1 2 3] [3 4 5] [8 2 3]) [1]

    ;; duplication in indata
    (set/difference [1 1 2 2]) [1 2]
    (set/difference [1 1 2 2] [2 3 3]) [1]))


(deftest intersection-test
  (are [x y] (= x y)
    ;; identity
    (set/intersection []) []
    (set/intersection [1]) [1]
    (set/intersection [1 2 3]) [1 2 3]

    ;; 2 sets, at least one is empty
    (set/intersection [] []) []
    (set/intersection [] [1]) []
    (set/intersection [] [1 2 3]) []
    (set/intersection [1] []) []
    (set/intersection [1 2 3] []) []

    ;; 2 sets
    (set/intersection [1 2] [1 2]) [1 2]
    (set/intersection [1 2] [3 4]) []
    (set/intersection [1 2] [1]) [1]
    (set/intersection [1 2] [2]) [2]
    (set/intersection [1 2 4] [2 3 4 5]) [2 4]

    ;; 3 sets, some are empty
    (set/intersection [] [] []) []
    (set/intersection [1] [] []) []
    (set/intersection [1] [1] []) []
    (set/intersection [1] [] [1]) []
    (set/intersection [1 2] [2 3] []) []

    ;; 3 sets
    (set/intersection [1 2] [2 3] [5 2]) [2]
    (set/intersection [1 2 3] [1 3 4] [1 3]) [1 3]
    (set/intersection [1 2 3] [3 4 5] [8 2 3]) [3]

    ;; duplication in indata
    (set/intersection [1 1 2 2] [2 3 3]) [2]))


(deftest union-test
  (are [x y] (= x y)
    (set/union) #{}

    ;; identity
    (set/union []) []
    (set/union [1]) [1]
    (set/union [1 2 3]) [1 2 3]

    ;; 2 sets, at least one is empty
    (set/union [] []) []
    (set/union [] [1]) [1]
    (set/union [] [1 2 3]) [1 2 3]
    (set/union [1] []) [1]
    (set/union [1 2 3] []) [1 2 3]

    ;; 2 sets
    (set/union [1] [2]) [1 2]
    (set/union [1] [1 2]) [1 2]
    (set/union [2] [1 2]) [2 1]
    (set/union [1 2] [3]) [1 2 3]
    (set/union [1 2] [2 3]) [1 2 3]

    ;; 3 sets, some are empty
    (set/union [] [] []) []
    (set/union [1] [] []) [1]
    (set/union [] [1] []) [1]
    (set/union [] [] [1]) [1]
    (set/union [1 2] [2 3] []) [1 2 3]

    ;; 3 sets
    (set/union [1 2] [3 4] [5 6]) [1 2 3 4 5 6]
    (set/union [1 2] [2 3] [1 3 4]) [1 2 3 4]

    ;; different data types
    (set/union [1 2] [:a :b] [nil] [false true] [\c "abc"] [[] [1 2]]
               [{} [:a 1]] [#{} #{1 2}])
    [1 2 :a :b nil false true \c "abc" [] [1 2] {} [:a 1] #{} #{1 2}]

    ;; duplication in indata
    (set/union [1 1 2 2] [2 3 3]) [1 2 3]))


(deftest restrict-test
  (are [x y] (= x y)
    (set/restrict [] integer?) []
    (set/restrict [1 2] integer?) [1 2]
    (set/restrict [1 2 :a :b :c] integer?) [1 2]
    (set/restrict [:a :b :c] integer?) []

    ;; duplication in indata
    (set/restrict [1 1 2 2] even?) [2]))


(deftest select-test
  (are [x y] (= x y)
    (set/select integer? []) []
    (set/select integer? [1 2]) [1 2]
    (set/select integer? [1 2 :a :b :c]) [1 2]
    (set/select integer? [:a :b :c]) []

    ;; duplication in indata
    (set/select even? [1 1 2 2]) [2]))


(deftest project-test
  (are [x y] (= x y)
    (set/project compositions [:name])
    [{:name "Art of the Fugue"}
     {:name "Musical Offering"}
     {:name "Requiem"}]

    (set/project compositions [:composer])
    [{:composer "J. S. Bach"}
     {:composer "Giuseppe Verdi"}
     {:composer "W. A. Mozart"}]

    (set/project compositions [:year]) [{}]
    (set/project [{}] [:name]) [{}]))


(deftest rename-test
  (are [x y] (= x y)
    (set/rename compositions {:name :title})
    [{:title "Art of the Fugue" :composer "J. S. Bach"}
     {:title "Musical Offering" :composer "J. S. Bach"}
     {:title "Requiem" :composer "Giuseppe Verdi"}
     {:title "Requiem" :composer "W. A. Mozart"}]

    (set/rename compositions {:year :decade})
    [{:name "Art of the Fugue" :composer "J. S. Bach"}
     {:name "Musical Offering" :composer "J. S. Bach"}
     {:name "Requiem" :composer "Giuseppe Verdi"}
     {:name "Requiem" :composer "W. A. Mozart"}]

    (set/rename [{}] {:year :decade})
    [{}]))


(deftest join-test
  (are [x y] (= x y)
    ;;
    ;; noop
    ;;

    (set/join compositions compositions)
    compositions

    ;;
    ;; natural join with empty set
    ;;

    (set/join compositions [])
    []

    ;;
    ;; natural join with matching keys
    ;;

    (set/join compositions [{:name "Art of the Fugue" :genre "Classical"}])
    [{:name "Art of the Fugue" :composer "J. S. Bach" :genre "Classical"}]

    (set/join compositions [{:composer "J. S. Bach" :genre "Classical"}])
    [{:name "Art of the Fugue" :composer "J. S. Bach" :genre "Classical"}
     {:name "Musical Offering" :composer "J. S. Bach" :genre "Classical"}]

    ;;
    ;; natural join with no matching keys -> cartesian product / cross join
    ;;

    (set/join compositions [{:composer-name "Someone" :genre "Classical"}])
    [{:name "Art of the Fugue" :composer "J. S. Bach"     :composer-name "Someone" :genre "Classical"}
     {:name "Musical Offering" :composer "J. S. Bach"     :composer-name "Someone" :genre "Classical"}
     {:name "Requiem"          :composer "Giuseppe Verdi" :composer-name "Someone" :genre "Classical"}
     {:name "Requiem"          :composer "W. A. Mozart"   :composer-name "Someone" :genre "Classical"}]

    ;;
    ;; key join with matching keys and no matching values
    ;;
    (set/join compositions
              [{:composer-name "Taylor Swift" :genre "Pop"}]
              {:composer :composer-name})
    []

    ;;
    ;; key join with matching keys and matching values
    ;;
    (set/join compositions
              [{:composer-name "J. S. Bach" :genre "Classical"}]
              {:composer :composer-name})
    [{:name "Art of the Fugue" :composer "J. S. Bach" :composer-name "J. S. Bach" :genre "Classical"}
     {:name "Musical Offering" :composer "J. S. Bach" :composer-name "J. S. Bach" :genre "Classical"}]))


(deftest update-test
  (are [x y] (= x y)
    (set/update compositions :name str/upper-case)
    [{:name "ART OF THE FUGUE" :composer "J. S. Bach"}
     {:name "MUSICAL OFFERING" :composer "J. S. Bach"}
     {:name "REQUIEM" :composer "Giuseppe Verdi"}
     {:name "REQUIEM" :composer "W. A. Mozart"}]))


