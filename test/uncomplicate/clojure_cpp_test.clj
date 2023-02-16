(ns uncomplicate.clojure-cpp-test
  (:require [midje.sweet :refer [facts throws => roughly]]
            [uncomplicate.commons.core :refer [info release]]
            [uncomplicate.clojure-cpp :refer :all]))

(facts
 "Test system info functions."
 (let [pb (physical-bytes)
       apb (available-physical-bytes)
       mpb (max-physical-bytes)
       tpb (total-physical-bytes)
       tb (tracked-bytes)
       mtb (max-tracked-bytes)
       pc (pointers-count)]
   (< 0 pb mpb) => true
   (<= 0 apb mpb) => true
   (<= 0 tb) => true
   (< tb mtb) => true
   (<= 0 pc tb) => true
   (<= pb (physical-bytes pb)) => true))

(facts
 "Test memory (de)allocation."
 (let [p (pointer nil)
       p1 (malloc! 16)
       p2 (calloc! 2 8)]
   (null? p) => true
   (nil? p) => false
   (nil? (malloc! -1)) => true
   (nil? (calloc! -2 -1)) => true
   (nil? (calloc! -2 1)) => true
   (nil? (calloc! 1 -2)) => true
   (info p) => {:address 0 :capacity 0 :deallocator nil :limit 0 :position 0 :type :default}
   (dissoc (info p1) :address) => {:capacity 0 :deallocator nil :limit 0 :position 0 :type :default}
   (pointer-seq p) => nil
   (pointer-seq p1) => '()
   (pointer-seq p2) => '()
   (free! p) => p
   (null? p1) => false
   (release p1) => true
   (null? p1) => true
   (null? p2) => false
   (free! p2) => p2
   (null? p2) => true
   (release p2) => true))

(defn test-array-pointer [constructor cast]
  (facts
   (format "Test %s pointer." cast)
   (let [bp (constructor 3)
         bp1 (constructor bp)
         bp2 (constructor [100 101 102])]
     (constructor -1) => (throws Exception)
     (null? bp) => false
     (count (pointer-seq bp)) => 3
     (count (pointer-seq bp1)) => 3
     (get-entry bp 0) =not=> (cast 0)
     (get-entry bp1 0) =not=> (cast 0)
     (zero! bp) => bp
     (get-entry bp1 0) => (cast 0)
     (put! bp 0 1) => bp
     (get-entry bp 0) => (cast 1)
     (get-entry bp1 0) => (cast 1)
     (zero! bp) => bp
     (get-entry bp 0) => (cast 0)
     (put! bp1 [10 20 30]) => bp1
     (pointer-seq bp) => (map cast [10 20 30])
     (pointer-seq bp2) => (map cast [100 101 102])
     (release bp) => true
     (release bp1) => true)))

(test-array-pointer byte-pointer byte)
(test-array-pointer char-pointer char)
(test-array-pointer short-pointer short)
(test-array-pointer int-pointer int)
(test-array-pointer long-pointer long)
(test-array-pointer float-pointer float)
(test-array-pointer double-pointer double)
(test-array-pointer size-t-pointer long)
(test-array-pointer size-t-pointer int)
(test-array-pointer clong-pointer long)
(test-array-pointer clong-pointer int)
