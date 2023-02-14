(ns uncomplicate.clojure-cpp-test
  (:require [midje.sweet :refer [facts throws => roughly]]
            [uncomplicate.clojure-cpp :refer :all]))

(facts "Test system info functions."
 (let [pb (physical-bytes)
       apb (available-physical-bytes)
       mpb (max-physical-bytes)
       tpb (total-physical-bytes)
       tb (tracked-bytes)
       mtb (max-tracked-bytes)
       pc (pointers-count)]
   (< 0 pb mpb) => true
   (<= 0 apb mpb) => true
   (= 0 tb) => true
   (< tb mtb) => true
   (= 0 pc) => true
   (physical-bytes pb) => pb))

(facts "Test memory (de)allocation."
       )
