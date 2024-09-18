(ns fooheads.setish
  (:refer-clojure :exclude [extend update])
  (:require
    [clojure.set :as set]
    [fooheads.stdlib :as std :refer [map-vals throw-ex]]))


(defn setish
  "Return a coll of the same type as xs but without duplicates."
  [xs]
  (if (set? xs)
    xs
    (std/into (empty xs) (distinct xs))))


(def map-invert
  "Returns the map with the vals mapped to the keys."
  set/map-invert)


(def rename-keys
  "Returns the map with the keys in kmap renamed to the vals in kmap"
  set/rename-keys)


(defn subset?
  "Is coll1 a subset of coll2"
  [coll1 coll2]
  (set/subset? (set (setish coll1)) (set (setish coll2))))


(defn superset?
  "Is coll1 a superset of coll2?"
  [coll1 coll2]
  (set/superset? (set (setish coll1)) (set (setish coll2))))


(defn index
  "Returns a map of the distinct values of ks in the xrel mapped to a
  set of the maps in xrel with the corresponding values of ks."
  [xrel ks]
  (map-vals
    #(into (empty xrel) %)
    (group-by #(select-keys % ks) (setish xrel))))


(defn index-unique
  "Like `index` but requires that each key only have one value. The vals in the
  index is a single value and not in a collection like with `index`. Throws
  if the unique constraint is not met."
  [xrel ks]
  (let [index (index xrel ks)]
    (->>
      index
      (map
        (fn [[k xs]]
          (cond
            (not= 1 (count xs)) (throw-ex "Not exactly one value for key {k}"
                                          xrel k ks xs)
            :else [k (first xs)])))
      (into {}))))


(defn difference
  "Return a coll that is the first coll without elements of the remaining colls"
  ([s1] (setish s1))
  ([s1 s2]
   (let [s2set (set s2)]
     (reduce
       (fn [rel x]
         (if (s2set x)
           rel
           (conj rel x)))
       (empty s1)
       (setish s1))))
  ([s1 s2 & sets]
   (reduce
     #(difference %1 %2)
     (difference s1 s2)
     sets)))


(defn intersection
  "Return a coll that is the intersection of the input colls"
  ([s1] (setish s1))
  ([s1 s2]
   (let [xset (set s1)
         yset (set s2)]

     (reduce
       (fn [rel v]
         (let [relset (set rel)] ; optimize
           (if (and (xset v) (yset v) (not (relset v)))
             (conj rel v)
             rel)))
       (empty s1)
       (concat (intersection s1) (intersection s2)))))
  ([s1 s2 & sets]
   (reduce
     #(intersection %1 %2)
     (intersection s1 s2)
     sets)))


(defn union
  "Return a coll that is the union of the input colls"
  ([] (set/union))
  ([s1] (setish s1))
  ([s1 s2]
   (into (union s1) (difference s2 s1)))
  ([s1 s2 & sets]
   (reduce
     #(union %1 %2)
     (union s1 s2)
     sets)))


(defn restrict
  "Returns a coll of the elements for which pred is true. Same as `select` but
  with the rel in first position to be more useful in a -> chain."
  [xrel pred?]
  (reduce
    (fn [rel x]
      (if (pred? x)
        (conj rel x)
        rel))
    (empty xrel)
    (setish xrel)))


(defn select
  "Returns a coll of the elements for which pred is true"
  [pred? xrel]
  (restrict xrel pred?))


(defn project
  "Returns a rel of the elements of xrel with only the keys in ks"
  [xrel attr-names]
  (setish
    (reduce
      (fn [rel x]
        (conj rel (select-keys x attr-names)))
      (empty xrel)
      xrel)))


(defn rename
  "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap"
  [xrel kmap]
  (into (empty xrel) (map #(rename-keys % kmap) xrel)))


(defn join
  "When passed 2 rels, returns the rel corresponding to the natural
  join. When passed an additional keymap, joins on the corresponding
  keys."
  ([xrel yrel]
   (let [join-keys (intersection (keys (first xrel)) (keys (first yrel)))
         km (into {} (map (fn [k] [k k]) join-keys))]
     (join xrel yrel km)))

  ([xrel yrel km]
   (let [index (index yrel (vals km))]
     (reduce
       (fn [rel x]
         (let [xprojection (select-keys x (keys km))
               ys (get index (rename-keys xprojection km))]
           (reduce
             (fn [rel y]
               (let [merged (merge x y)]
                 (conj rel merged)))
             rel
             ys)))
       (empty xrel)
       (setish xrel)))))


(defn left-join
  "Joins, but always keeps enerything on the left, even if there is no
  match on the right. Each row will contain all keys from the left, plus
  all keys in the km (possibly with nil values), and, when there is a match,
  all keys from the right."
  ([xrel yrel km]
   (let [index (index yrel (vals km))
         blank-left (zipmap (keys km) (repeat nil))]
     (reduce
       (fn [rel x]
         (let [xprojection (merge blank-left (select-keys x (keys km)))
               yprojection (rename-keys xprojection km)
               ys (or (get index yprojection)
                      (zipmap (keys yprojection) (repeat nil)))]
           (reduce
             (fn [rel y]
               (let [merged (merge x y)]
                 (conj rel merged)))
             rel
             ys)))
       (empty xrel)
       (setish xrel)))))


(defn update
  "Like clojure.core/update, but works on each tuple in a rel."
  ([xrel k f & args]
   (std/mapt (fn [tuple] (apply clojure.core/update tuple k f args)) xrel)))


(defn extend
  "Extends each tuple by assoc:ing k to the result of (f tuple).
  k can either be a new key or an existing key.

  A map from k to f can also be specified, to extend multiple
  attributes in one call."

  ([xrel m]
   (std/mapt
     (fn [tuple]
       (reduce
         (fn [tuple [k f]]
           (assoc tuple k (f tuple)))
         tuple
         m))
     xrel))
  ([xrel k f]
   (std/mapt (fn [tuple] (assoc tuple k (f tuple))) xrel)))


(defn order-by
  "Orders the tuples according to keyfn and an optional comparator.
  Same behavior as `clojure.core/sort-by`, but with xrel in first position."
  ([xrel keyfn]
   (sort-by keyfn xrel))
  ([xrel keyfn comp]
   (sort-by keyfn comp xrel)))


(defn aggregate
  "Applies f to the xrel and returns a relation with one tuple.

  In the case a map is provided in place of k and f, the same
  procedure will be applied to all pairs of k and f in m."
  ([xrel k f]
   (aggregate xrel {k f}))

  ([xrel m]
   (->>
     m
     (mapv (fn [[k f]] [k (f xrel)]))
     (into {})
     (std/conjt (std/empty xrel)))))


(defn- aggregate-by-map
  [xrel agg-ks m]

  (->>
    (index xrel agg-ks)
    (mapv
      (fn [[index-key tuples]]
        (reduce
          (fn [tuple [k f]]
            (assoc tuple k (f tuples)))
          index-key
          m)))

    (into (std/empty xrel))))


(defn- aggregate-by-fn
  [xrel agg-ks f]

  (->>
    (index xrel agg-ks)
    (mapcat
      (fn [[_ tuples]]
        (f tuples)))

    (into (std/empty xrel))))


(defn aggregate-by
  "Groups by agg-ks and aggregates the grouped relations.

  If a function is provided, it is expected to take a relation
  and return a relation"
  ([xrel agg-ks k f]
   (aggregate-by xrel agg-ks {k f}))

  ([xrel agg-ks m-or-f]
   (cond
     (map? m-or-f)
     (aggregate-by-map xrel agg-ks m-or-f)

     (fn? m-or-f)
     (aggregate-by-fn xrel agg-ks m-or-f))))


(defn sum
  "Convenience function to use with set aggregations."
  [k]
  (fn [xrel]
    (reduce + 0 (map k xrel))))

