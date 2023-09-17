# setish

Set operations such as in
[clojure.set](https://clojuredocs.org/clojure.set) that also works for
vectors and keeps order, in a similar that it works for set. 

Note: not a good support for lists yet.

Apart from the functions from clojure.set, this namespace have two extra
functions:

    * setish - returns a collection without duplicates
    * restrict - same as select, but with xrel in fist position to play
    well with (-> ...)
    * update 
    * index-unique

