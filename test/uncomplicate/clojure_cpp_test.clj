(ns uncomplicate.clojure-cpp-test
  (:require [midje.sweet :refer [facts throws => roughly]]
            [uncomplicate.commons.core :refer [info release with-release size bytesize sizeof]]
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
   (null? p1) => false
   (null? p2) => false
   (free! p2) => p2
   (null? p2) => true
   (release p2) => true))

(facts
 "Test memcpy."
 (with-release [p (long-pointer 3)
                p1 (long-pointer 3)]
   (nil? p) => false
   (fill! p 100) => p
   (pointer-seq p1) =not=> (pointer-seq p)
   (memcpy! p p1 16) => p1
   (pointer-seq p1) =not=> (pointer-seq p)
   (memcpy! p p1 24) => p1
   (pointer-seq p1) => (pointer-seq p)))

(defn test-array-pointer [constructor cast array]
  (facts
   (format "Test %s pointer." cast)
   (let [bp (constructor 3)
         bp1 (constructor bp)
         bp2 (constructor [100 101 102])
         bp3 (constructor (array (map cast [100 101 102])))]
     (constructor -1) => (throws Exception)
     (null? bp) => false
     (count (pointer-seq bp)) => 3
     (count (pointer-seq bp1)) => 3
     (get-entry bp 0) =not=> (cast 0)
     (get-entry bp1 0) =not=> (cast 0)
     (zero! bp) => bp
     (get-entry bp1 0) => (cast 0)
     (put-entry! bp 0 1) => bp
     (get-entry bp 0) => (cast 1)
     (get-entry bp1 0) => (cast 1)
     (zero! bp) => bp
     (get-entry bp 0) => (cast 0)
     (put! bp1 [10 20 30]) => bp1
     (seq (get! bp1 (array 3))) => (map cast [10 20 30])
     (pointer-seq bp) => (map cast [10 20 30])
     (pointer-vec bp) => (vec (pointer-seq bp))
     (pointer-seq bp2) => (map cast [100 101 102])
     bp3 =not=> bp2
     (release bp) => true
     (release bp1) => true
     (release bp2) => true
     (release bp3) => true)))

(test-array-pointer byte-pointer byte byte-array)
(test-array-pointer char-pointer char char-array)
(test-array-pointer short-pointer short short-array)
(test-array-pointer int-pointer int int-array)
(test-array-pointer long-pointer long long-array)
(test-array-pointer float-pointer float float-array)
(test-array-pointer double-pointer double double-array)
(test-array-pointer size-t-pointer long long-array)
(test-array-pointer size-t-pointer int long-array)
(test-array-pointer clong-pointer long long-array)
(test-array-pointer clong-pointer int long-array)

(facts
 "Test PointerPointer"
 (let [pp (pointer-pointer 3)
       dp (double-pointer [1 2 3 4 5])
       ip (int-pointer [10 20 30])
       s (pointer "Just some random string")]
   (put-entry! pp 2 s) => pp
   (get-entry pp 2) => s
   (put-entry! pp 1 dp) => pp
   (get-entry (double-pointer (get-entry pp 1)) 3) => 4.0
   (put-entry! pp 0 ip) => pp
   (get-entry (int-pointer (get-entry pp 0)) 1) => 20
   (release pp) => true
   (null? pp) => true))

(facts
 "Test PointerPointer from sequence."
 (let [dp (double-pointer [1 2 3 4 5])
       ip (int-pointer [10 20 30])
       s "Just some random string"
       pp (pointer-pointer [ip dp s])]
   (get-string (byte-pointer (get-entry pp 2))) => s
   (get-entry (double-pointer (get-entry pp 1)) 3) => 4.0
   (get-entry (int-pointer (get-entry pp 0)) 1) => 20
   (release pp) => true
   (null? pp) => true))

(facts
 "Test bulk put-entry!"
 (with-release [p11 (byte-pointer 11)
                p3 (int-pointer 3)]
   (fill! p11 2) => p11
   (pointer-seq (fill! p3 Integer/MAX_VALUE)) => (repeat 3 Integer/MAX_VALUE)
   (pointer-seq (put-entry! p11 (int 111))) => [111 0 0 0 2 2 2 2 2 2 2]))

(facts
 "Test Pointer coercion properties."
 (with-release [bp (byte-pointer 64)
                lp (long-pointer bp)
                bp1 (byte-pointer lp)]
   (size bp) => 64
   (bytesize bp) => 64
   (sizeof bp) => 1
   (size lp) => 8
   (bytesize lp) => 64
   (sizeof lp) => 8
   (dotimes [i 8]
     (put-long! bp i (inc i))
     (get-long bp i) => (inc i)
     (get-entry lp i) => (inc i))
   (position lp) => 0
   (capacity lp) => 8
   (limit lp) => 8
   (position! lp 1) => lp
   (position lp) => 1
   (capacity! lp 6) => lp
   (capacity lp) => 6
   (limit! lp 5) => lp
   (limit lp) => 5
   (size lp) => 4
   (bytesize lp) => 32
   (position bp1) => 0
   (let [bp2 (byte-pointer lp)]
     (position bp2) => (* (sizeof lp) (position lp))
     (capacity bp2) => (* (sizeof lp) (capacity lp))
     (limit bp2) => (* (sizeof lp) (limit lp)))
   (let [bp3 (get-pointer lp :byte 0)]
     (position bp3) => (* (sizeof lp) (position lp))
     (capacity bp3) => (* (sizeof lp) (capacity lp))
     (limit bp3) => (* (sizeof lp) (limit lp)))))
