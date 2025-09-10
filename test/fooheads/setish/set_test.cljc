(ns fooheads.setish.set-test
  "Tests for the set compatibility of setish. Most of the test code here is
  a clone from the tests for clojure.set in order to ensure compatibility.

  https://github.com/clojure/clojure/blob/c89bf2e9bcfc1bca62e36dee2d78a48f8c38c15c/test/clojure/test_clojure/clojure_set.clj
  "
  (:require
    [clojure.string :as str]
    [clojure.test :refer [are deftest is testing]]
    [fooheads.setish :as set]
    [fooheads.stdlib :as std]))


(def compositions
  #{{:name "Art of the Fugue" :composer "J. S. Bach"}
    {:name "Musical Offering" :composer "J. S. Bach"}
    {:name "Requiem" :composer "Giuseppe Verdi"}
    {:name "Requiem" :composer "W. A. Mozart"}})


(deftest subset?-test
  (are [sub super] (set/subset? sub super)
    #{}           #{}
    #{}           #{1}
    #{1}          #{1}
    #{1 2}        #{1 2}
    #{1 2}        #{1 2 42}
    #{false}      #{false}
    #{nil}        #{nil}
    #{nil}        #{nil false}
    #{1 2 nil}    #{1 2 nil 4})
  (are [notsub super] (not (set/subset? notsub super))
    #{1}          #{}
    #{2}          #{1}
    #{1 3}        #{1}
    #{nil}        #{false}
    #{false}      #{nil}
    #{false nil}  #{nil}
    #{1 2 nil}    #{1 2}))


(deftest superset?-test
  (are [super sub] (set/superset? super sub)
    #{}                 #{}
    #{1}                #{}
    #{1}                #{1}
    #{1 2}              #{1 2}
    #{1 2 42}           #{1 2}
    #{false}            #{false}
    #{nil}              #{nil}
    #{false nil}        #{false}
    #{1 2 4 nil false}  #{1 2 nil})
  (are [notsuper sub] (not (set/superset? notsuper sub))
    #{}                 #{1}
    #{2}                #{1}
    #{1}                #{1 3}
    #{nil}              #{false}
    #{false}            #{nil}
    #{nil}              #{false nil}
    #{nil 2 3}          #{false nil 2 3}))


(deftest index-test
  (are [x y] (= x y)
    (set/index
      #{{:c 2} {:b 1} {:a 1 :b 2}}
      [:b])

    {{:b 1} #{{:b 1}}
     {:b 2} #{{:a 1 :b 2}},
     {}     #{{:c 2}}}))


(deftest difference-test
  (are [x y] (= x y)
    ;; identity
    (set/difference #{}) #{}
    (set/difference #{1}) #{1}
    (set/difference #{1 2 3}) #{1 2 3}

    ;; 2 sets
    (set/difference #{1 2} #{1 2}) #{}
    (set/difference #{1 2} #{3 4}) #{1 2}
    (set/difference #{1 2} #{1}) #{2}
    (set/difference #{1 2} #{2}) #{1}
    (set/difference #{1 2 4} #{2 3 4 5}) #{1}

    ;; 3 sets
    (set/difference #{1 2} #{2 3} #{5 2}) #{1}
    (set/difference #{1 2 3} #{1 3 4} #{1 3}) #{2}
    (set/difference #{1 2 3} #{3 4 5} #{8 2 3}) #{1}))


(deftest intersection-test
  (are [x y] (= x y)
    ;; identity
    (set/intersection #{}) #{}
    (set/intersection #{1}) #{1}
    (set/intersection #{1 2 3}) #{1 2 3}

    ;; 2 sets, at least one is empty
    (set/intersection #{} #{}) #{}
    (set/intersection #{} #{1}) #{}
    (set/intersection #{} #{1 2 3}) #{}
    (set/intersection #{1} #{}) #{}
    (set/intersection #{1 2 3} #{}) #{}

    ;; 2 sets
    (set/intersection #{1 2} #{1 2}) #{1 2}
    (set/intersection #{1 2} #{3 4}) #{}
    (set/intersection #{1 2} #{1}) #{1}
    (set/intersection #{1 2} #{2}) #{2}
    (set/intersection #{1 2 4} #{2 3 4 5}) #{2 4}

    ;; 3 sets, some are empty
    (set/intersection #{} #{} #{}) #{}
    (set/intersection #{1} #{} #{}) #{}
    (set/intersection #{1} #{1} #{}) #{}
    (set/intersection #{1} #{} #{1}) #{}
    (set/intersection #{1 2} #{2 3} #{}) #{}

    ;; 3 sets
    (set/intersection #{1 2} #{2 3} #{5 2}) #{2}
    (set/intersection #{1 2 3} #{1 3 4} #{1 3}) #{1 3}
    (set/intersection #{1 2 3} #{3 4 5} #{8 2 3}) #{3}

    ;; different types of sets
    (set/intersection (hash-set 1 2) (hash-set 2 3)) #{2}

    (set/intersection (sorted-set 1 2) (sorted-set 2 3)) #{2}

    (set/intersection
      (hash-set 1 2) (hash-set 2 3)
      (sorted-set 1 2) (sorted-set 2 3)) #{2}))


(deftest union-test
  (are [x y] (= x y)
    (set/union) #{}

    ;; identity
    (set/union #{}) #{}
    (set/union #{1}) #{1}
    (set/union #{1 2 3}) #{1 2 3}

    ;; 2 sets, at least one is empty
    (set/union #{} #{}) #{}
    (set/union #{} #{1}) #{1}
    (set/union #{} #{1 2 3}) #{1 2 3}
    (set/union #{1} #{}) #{1}
    (set/union #{1 2 3} #{}) #{1 2 3}

    ;; 2 sets
    (set/union #{1} #{2}) #{1 2}
    (set/union #{1} #{1 2}) #{1 2}
    (set/union #{2} #{1 2}) #{1 2}
    (set/union #{1 2} #{3}) #{1 2 3}
    (set/union #{1 2} #{2 3}) #{1 2 3}

    ;; 3 sets, some are empty
    (set/union #{} #{} #{}) #{}
    (set/union #{1} #{} #{}) #{1}
    (set/union #{} #{1} #{}) #{1}
    (set/union #{} #{} #{1}) #{1}
    (set/union #{1 2} #{2 3} #{}) #{1 2 3}

    ;; 3 sets
    (set/union #{1 2} #{3 4} #{5 6}) #{1 2 3 4 5 6}
    (set/union #{1 2} #{2 3} #{1 3 4}) #{1 2 3 4}

    ;; different data types
    (set/union #{1 2} #{:a :b} #{nil} #{false true} #{\c "abc"} #{[] [1 2]}
               #{{} {:a 1}} #{#{} #{1 2}})
    #{1 2 :a :b nil false true \c "abc" [] [1 2] {} {:a 1} #{} #{1 2}}

    ;; different types of sets
    (set/union (hash-set) (hash-set 1 2) (hash-set 2 3))
    (hash-set 1 2 3)

    (set/union (sorted-set) (sorted-set 1 2) (sorted-set 2 3))
    (sorted-set 1 2 3)

    (set/union (hash-set) (hash-set 1 2) (hash-set 2 3)
               (sorted-set) (sorted-set 4 5) (sorted-set 5 6))
    (hash-set 1 2 3 4 5 6)))  ; also equals (sorted-set 1 2 3 4 5 6)


(deftest restrict-test
  (are [x y] (= x y)
    (set/restrict #{} integer?) #{}
    (set/restrict #{1 2} integer?) #{1 2}
    (set/restrict #{1 2 :a :b :c} integer?) #{1 2}
    (set/restrict #{:a :b :c} integer?) #{}))


(deftest select-test
  (are [x y] (= x y)
    (set/select integer? #{}) #{}
    (set/select integer? #{1 2}) #{1 2}
    (set/select integer? #{1 2 :a :b :c}) #{1 2}
    (set/select integer? #{:a :b :c}) #{}))


(deftest project-test
  (are [x y] (= x y)
    (set/project compositions [:name])
    #{{:name "Art of the Fugue"}
      {:name "Requiem"}
      {:name "Musical Offering"}}

    (set/project compositions [:composer])
    #{{:composer "W. A. Mozart"}
      {:composer "Giuseppe Verdi"}
      {:composer "J. S. Bach"}}

    (set/project compositions [:year])
    #{{}}

    (set/project #{{}} [:name])
    #{{}}))


(deftest project-away-test
  (are [x y] (= x y)
    (set/project-away compositions [:composer])
    #{{:name "Art of the Fugue"}
      {:name "Musical Offering"}
      {:name "Requiem"}}

    (set/project-away compositions [:name])
    #{{:composer "J. S. Bach"}
      {:composer "Giuseppe Verdi"}
      {:composer "W. A. Mozart"}}

    (set/project-away compositions [:composer :name]) #{{}}
    (set/project-away #{{}} [:name]) #{{}}))


(deftest rename-test
  (are [x y] (= x y)
    (set/rename compositions {:name :title})
    #{{:title "Art of the Fugue" :composer "J. S. Bach"}
      {:title "Musical Offering" :composer "J. S. Bach"}
      {:title "Requiem" :composer "Giuseppe Verdi"}
      {:title "Requiem" :composer "W. A. Mozart"}}

    (set/rename compositions {:year :decade})
    #{{:name "Art of the Fugue" :composer "J. S. Bach"}
      {:name "Musical Offering" :composer "J. S. Bach"}
      {:name "Requiem" :composer "Giuseppe Verdi"}
      {:name "Requiem" :composer "W. A. Mozart"}}

    (set/rename #{{}} {:year :decade})
    #{{}}))


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

    (set/join compositions #{})
    #{}

    ;;
    ;; natural join with matching keys
    ;;

    (set/join compositions #{{:name "Art of the Fugue" :genre "Classical"}})
    #{{:name "Art of the Fugue" :composer "J. S. Bach" :genre "Classical"}}

    (set/join compositions #{{:composer "J. S. Bach" :genre "Classical"}})
    #{{:name "Art of the Fugue" :composer "J. S. Bach" :genre "Classical"}
      {:name "Musical Offering" :composer "J. S. Bach" :genre "Classical"}}

    ;;
    ;; natural join with no matching keys -> cartesian product / cross join
    ;;

    (set/join compositions #{{:composer-name "Someone" :genre "Classical"}})
    #{{:name "Art of the Fugue" :composer "J. S. Bach"     :composer-name "Someone" :genre "Classical"}
      {:name "Musical Offering" :composer "J. S. Bach"     :composer-name "Someone" :genre "Classical"}
      {:name "Requiem"          :composer "Giuseppe Verdi" :composer-name "Someone" :genre "Classical"}
      {:name "Requiem"          :composer "W. A. Mozart"   :composer-name "Someone" :genre "Classical"}}

    ;;
    ;; key join with matching keys and no matching values
    ;;
    (set/join compositions
              #{{:composer-name "Taylor Swift" :genre "Pop"}}
              {:composer :composer-name})
    #{}

    ;;
    ;; key join with matching keys and matching values
    ;;
    (set/join compositions
              #{{:composer-name "J. S. Bach" :genre "Classical"}}
              {:composer :composer-name})
    #{{:composer-name "J. S. Bach" :genre "Classical" :name "Art of the Fugue" :composer "J. S. Bach"}
      {:composer-name "J. S. Bach" :genre "Classical" :name "Musical Offering" :composer "J. S. Bach"}}

    ;;
    ;; key join should be distinct
    ;;
    (set/join compositions compositions {:composer :composer})
    compositions))


(deftest left-join-test
  (are [x y] (= x y)
    ;;
    ;; key join with matching keys and no matching values
    ;;
    (set/left-join
      compositions
      #{{:composer-name "Taylor Swift" :genre "Pop"}}
      {:composer :composer-name})

    #{{:name "Art of the Fugue" :composer "J. S. Bach" :composer-name nil}
      {:name "Musical Offering" :composer "J. S. Bach" :composer-name nil}
      {:name "Requiem" :composer "Giuseppe Verdi" :composer-name nil}
      {:name "Requiem" :composer "W. A. Mozart" :composer-name nil}}

    ;;
    ;; key join with matching keys and some matching values
    ;;
    (set/left-join compositions
                   #{{:composer-name "J. S. Bach" :genre "Classical"}}
                   {:composer :composer-name})
    #{{:name "Art of the Fugue" :composer "J. S. Bach" :composer-name "J. S. Bach" :genre "Classical"}
      {:name "Musical Offering" :composer "J. S. Bach" :composer-name "J. S. Bach" :genre "Classical"}
      {:name "Requiem" :composer "Giuseppe Verdi" :composer-name nil}
      {:name "Requiem" :composer "W. A. Mozart" :composer-name nil}}

    ;;
    ;; match returning more rows that the left contains
    ;;
    (set/left-join
      #{{:composer-name "J. S. Bach" :genre "Classical"}
        {:composer-name "J. S. Bach" :genre "Baroque"}
        {:composer-name "Ludwig van Beethoven" :genre "Classical"}}
      compositions
      {:composer-name :composer})

    #{{:composer-name "J. S. Bach"           :genre "Classical" :name "Art of the Fugue" :composer "J. S. Bach"}
      {:composer-name "J. S. Bach"           :genre "Classical" :name "Musical Offering" :composer "J. S. Bach"}
      {:composer-name "J. S. Bach"           :genre "Baroque"   :name "Art of the Fugue" :composer "J. S. Bach"}
      {:composer-name "J. S. Bach"           :genre "Baroque"   :name "Musical Offering" :composer "J. S. Bach"}
      {:composer-name "Ludwig van Beethoven" :genre "Classical"                          :composer nil}}

    ;;
    ;; left-join with nil should be identity + joined key
    ;;
    (set/left-join
      compositions
      nil
      {:composer :composer-name})


    #{{:composer "J. S. Bach"     :name "Art of the Fugue"  :composer-name nil}
      {:composer "J. S. Bach"     :name "Musical Offering"  :composer-name nil}
      {:composer "Giuseppe Verdi" :name "Requiem"           :composer-name nil}
      {:composer "W. A. Mozart"   :name "Requiem"           :composer-name nil}}


    ;;
    ;; left-join with non-existing join key should be identity + joined key
    ;;
    (set/left-join
      compositions
      nil
      {:foo :composer-name})


    #{{:composer "J. S. Bach"     :name "Art of the Fugue"  :composer-name nil}
      {:composer "J. S. Bach"     :name "Musical Offering"  :composer-name nil}
      {:composer "Giuseppe Verdi" :name "Requiem"           :composer-name nil}
      {:composer "W. A. Mozart"   :name "Requiem"           :composer-name nil}}

    ;;
    ;; left-join when key exists on left but not on right should be left
    ;;
    (set/left-join
      compositions
      nil
      {:composer :composer})

    #{{:composer "J. S. Bach"     :name "Art of the Fugue"}
      {:composer "J. S. Bach"     :name "Musical Offering"}
      {:composer "Giuseppe Verdi" :name "Requiem"}
      {:composer "W. A. Mozart"   :name "Requiem"}}))


(deftest update-test
  (are [x y] (= x y)
    (set/update compositions :name str/upper-case)
    #{{:name "ART OF THE FUGUE" :composer "J. S. Bach"}
      {:name "MUSICAL OFFERING" :composer "J. S. Bach"}
      {:name "REQUIEM" :composer "Giuseppe Verdi"}
      {:name "REQUIEM" :composer "W. A. Mozart"}}))


(deftest extend-test
  (testing "single"
    (are [x y] (= x y)
      (set/extend
        compositions
        :short (fn [tuple]
                 (str (subs (:name tuple) 0 3) "...")))

      #{{:name "Art of the Fugue" :short "Art..." :composer "J. S. Bach"}
        {:name "Musical Offering" :short "Mus..." :composer "J. S. Bach"}
        {:name "Requiem"          :short "Req..." :composer "Giuseppe Verdi"}
        {:name "Requiem"          :short "Req..." :composer "W. A. Mozart"}}))

  (testing "multiple, map"
    (are [x y] (= x y)
      (set/extend
        compositions
        {:short (fn [tuple]
                  (str (subs (:name tuple) 0 3) "..."))})

      #{{:name "Art of the Fugue" :short "Art..." :composer "J. S. Bach"}
        {:name "Musical Offering" :short "Mus..." :composer "J. S. Bach"}
        {:name "Requiem"          :short "Req..." :composer "Giuseppe Verdi"}
        {:name "Requiem"          :short "Req..." :composer "W. A. Mozart"}}))

  (testing "multiple, ordered"
    (are [x y] (= x y)
      (set/extend
        compositions
        [[:short (fn [tuple]
                   (str (subs (:name tuple) 0 3) "..."))]])

      #{{:name "Art of the Fugue" :short "Art..." :composer "J. S. Bach"}
        {:name "Musical Offering" :short "Mus..." :composer "J. S. Bach"}
        {:name "Requiem"          :short "Req..." :composer "Giuseppe Verdi"}
        {:name "Requiem"          :short "Req..." :composer "W. A. Mozart"}})))


(deftest aggregate-test
  (testing "one aggregation"
    (is (= #{{:sum 6}}
           (set/aggregate
             #{{:rank 2} {:rank 3} {:rank 1}}
             :sum
             (set/sum :rank)))))

  (testing "multiple aggregations"
    (is (= #{{:sum 6 :count 3}}
           (set/aggregate
             #{{:rank 2} {:rank 3} {:rank 1}}
             {:sum (set/sum :rank)
              :count count})))))


(deftest aggregate-by-test
  (testing "one aggregation"
    (is (= #{{:count 2 :composer "J. S. Bach"}
             {:count 1 :composer "Giuseppe Verdi"}
             {:count 1 :composer "W. A. Mozart"}}

           (set/aggregate-by
             compositions
             [:composer]
             :count
             count))))

  (testing "multiple aggregations"
    (is (= #{{:composer "J. S. Bach"     :count 2 :num-chars 32}
             {:composer "Giuseppe Verdi" :count 1 :num-chars  7}
             {:composer "W. A. Mozart"   :count 1 :num-chars  7}}

           (set/aggregate-by
             compositions
             [:composer]
             {:count count
              :num-chars (fn [tuples]
                           (->> tuples (map (comp count :name)) (apply +)))}))))

  (testing "function aggregation"
    (is (= compositions
           (set/aggregate-by
             compositions
             [:composer]
             identity)))

    (is (= #{}
           (set/aggregate-by
             compositions
             [:composer]
             std/empty)))))

