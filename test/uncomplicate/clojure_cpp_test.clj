(ns uncomplicate.clojure-cpp-test
  (:require [midje.sweet :refer [facts throws => roughly]]
            [uncomplicate.commons
             [core :refer [info release with-release size bytesize sizeof]]
             [utils :as commons]]
            [uncomplicate.clojure-cpp :refer :all])
  (:import java.nio.IntBuffer))

(facts
 "Test pointer type mappings."
 (pointer-type org.bytedeco.javacpp.FloatPointer) => :float
 (pointer-type Float) => nil
 (pointer-class :double) => org.bytedeco.javacpp.DoublePointer
 (pointer-class Double/TYPE) => org.bytedeco.javacpp.DoublePointer
 (pointer-type (pointer-class :byte)) => :byte)

(facts
 "Test system info functions."
 (let [pb (physical-bytes)
       apb (available-physical-bytes)
       mpb (max-physical-bytes)
       tpb (total-physical-bytes)
       tb (tracked-bytes)
       mtb (max-tracked-bytes)
       pc (pointers-count)]
   (with-release [x (byte-pointer 10000)]
     (< tb (tracked-bytes)) => true
     (< 0 pb mpb) => true
     (<= 0 apb mpb) => true
     (<= 0 tb) => true
     (< tb mtb) => true
     (<= 0 pc tb) => true
     (<= pb (physical-bytes pb)) => true
     (<= pb tpb) => true)))

(facts
 "Test memory (de)allocation."
 (let [p (pointer nil)
       p1 (malloc! 16)
       p2 (calloc! 2 8)
       p3 (malloc! 128000)
       p4 (realloc! p1 1000)]
   (null? p) => true
   (nil? p) => false
   (nil? (malloc! -1)) => true
   (nil? (calloc! -2 -1)) => true
   (nil? (calloc! -2 1)) => true
   (nil? (calloc! 1 -2)) => true
   (info p) => {:address 0 :capacity 0 :deallocator nil :limit 0 :position 0 :type :default}
   (dissoc (info p1) :address) => {:capacity 16 :deallocator nil :limit 16 :position 0 :type :default}
   (pointer-seq p) => nil
   (pointer-seq p1) => (throws IllegalArgumentException)
   (pointer-seq p2) => (throws IllegalArgumentException)
   (size p1) => 16
   (bytesize p1) => 16
   (sizeof p1) => 1
   (size p2) => 16
   (bytesize p2) => 16
   (sizeof p2) => 1
   (free! p) => p
   (free! p) => p
   (null? p1) => false
   (release p1) => true
   (null? p1) => false
   (null? p2) => false
   (free! p2) => p2
   (null? p2) => true
   (release p2) => true
   (free! (position! p3 100000)) => p3
   (null? p3) => true
   (null? p4) => false
   (free! p4) => p4))

(facts
 "Test memcmp"
 (with-release [p1 (int-pointer [1 2 3])
                p2 (int-pointer [-1 3 3 5])
                p3 (pointer 0)]
   (memcmp p1 p2) => -254
   (memcmp p1 p3) => 1
   (memcmp p1 p1) => 0))

(facts
 "Test memcpy."
 (with-release [p (long-pointer [1 2 3])
                p1 (long-pointer 3)]
   (nil? p) => false
   (fill! p1 100) => p1
   (pointer-seq p1) =not=> (pointer-seq p)
   (memcpy! p1 p 0) => p
   (pointer-seq p1) =not=> (pointer-seq p)
   (memcpy! p p1 24) => p1
   (memcpy! p p1 48) => (throws IndexOutOfBoundsException)
   (memmove! p p1 48) => (throws IndexOutOfBoundsException)
   (pointer-seq p1) => (pointer-seq p)
   (pointer-vec (memcpy! (pointer p 1) (pointer p) 16)) => [2 3 3]
   (pointer-vec (memmove! p p1)) => [2 3 3]))

(facts
 "Test pointer properties."
 (with-release [p (malloc! 36)
                p1 (calloc! 2 4)
                int-p (int-pointer p)
                byte-p (byte-pointer [0 0 0 1 0 0 1 2])]
   (address p) => (address int-p)
   (null? p) => false
   (null? int-p) => false
   (position p) => 0
   (position int-p) => 0
   (capacity p) => 36
   (limit p) => (capacity p)
   (bytesize p) => 36
   (capacity int-p) => 9
   (limit int-p) => (capacity int-p)
   (capacity! p 37) => p
   (capacity int-p) => 9
   (capacity p1) => 8
   (pointer-vec (int-pointer p1)) => [0 0]
   (get-entry (get-pointer byte-p :int 1)) => 33619968
   (get-pointer (get-pointer byte-p :int 1) :byte 2) [1 2]))

(facts
 "Test safe."
 (with-release [p (pointer nil)
                int-p (int-pointer 3)]
   (safe nil) => (throws IllegalArgumentException)
   (safe p) => (throws IllegalArgumentException)
   (safe2 nil) => nil
   (safe2 p) => (throws IllegalArgumentException)
   (safe int-p) => int-p
   (safe2 int-p) => int-p
   (release int-p) => true
   (safe int-p) => (throws IllegalArgumentException)
   (safe2 int-p) => (throws IllegalArgumentException)))

(defn test-coercion [constructor cast other ptr* ptr ptr2]
  (facts
   "Test coercion functions."
   (with-release [p-123 (constructor [1 2 3])
                  p-0 (constructor 0)
                  p-nil (constructor nil)
                  p-other (other 3)]
     (null? p-nil) => true
     (ptr* nil) => nil
     (ptr* p-nil) => p-nil
     (ptr* p-0) => p-0
     (ptr* p-123) => p-123
     (ptr* p-123 0) => p-123
     (ptr* p-other) => p-other
     (ptr* p-other 0) => p-other
     (pointer-seq (ptr* p-123 -1)) => (map cast [1 2 3])
     (pointer-seq (ptr* p-123 0)) => (map cast [1 2 3])
     (pointer-seq (ptr* p-123 1)) => (map cast [2 3])
     (pointer-seq (ptr* p-123 3)) => (map cast [])
     (pointer-seq (ptr* p-123 4)) => (map cast [])
     (ptr nil) => (throws IllegalArgumentException)
     (ptr p-nil) => (throws IllegalArgumentException)
     (ptr p-0) => p-0
     (ptr p-123) => p-123
     (ptr p-other) => (throws ClassCastException)
     (ptr p-other 0) => (throws ClassCastException)
     (ptr2 nil) => nil
     (ptr2 p-nil) => (throws IllegalArgumentException)
     (ptr2 p-0) => p-0
     (ptr2 p-123) => p-123
     (ptr2 p-other) => (throws ClassCastException)
     (ptr2 p-other 0) => (throws ClassCastException))))

(test-coercion float-pointer float int-pointer float-ptr* float-ptr float-ptr2)
(test-coercion double-pointer double int-pointer double-ptr* double-ptr double-ptr2)
(test-coercion long-pointer long int-pointer long-ptr* long-ptr long-ptr2)
(test-coercion int-pointer int float-pointer int-ptr* int-ptr int-ptr2)
(test-coercion short-pointer short int-pointer short-ptr* short-ptr short-ptr2)
(test-coercion byte-pointer byte double-pointer byte-ptr* byte-ptr byte-ptr2)
(test-coercion char-pointer char double-pointer char-ptr* char-ptr char-ptr2)
(test-coercion bool-pointer boolean double-pointer bool-ptr* bool-ptr bool-ptr2)
(test-coercion size-t-pointer long double-pointer size-t-ptr* size-t-ptr size-t-ptr2)
(test-coercion clong-pointer long double-pointer clong-ptr* clong-ptr clong-ptr2)

(facts
 "Test buffer."
 (with-release [p (pointer nil)
                float-p (float-pointer 3)
                int-p (int-pointer 1)]
   (as-buffer p) => nil
   (as-byte-buffer p) => nil
   (let [int-buf (as-buffer int-p)
         byte-buf (as-byte-buffer int-p)]
     (put-entry! int-p 33) => int-p
     (.get ^IntBuffer int-buf 0) => 33
     (commons/get-int byte-buf 0) => 33
     (.put ^IntBuffer int-buf 0 3) => int-buf
     (get-entry int-p 0) => 3
     (release int-p) => true
     (.get ^IntBuffer int-buf 0) =not=> 3)))

(defn test-array-pointer [constructor cast array]
  (facts
   (format "Test %s pointer." cast)
   (let [bp (constructor 3)
         bp1 (constructor bp)
         bp2 (constructor [100 101 102])
         arr3 (array (map cast [100 101 102]))
         bp3 (constructor arr3)]
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
     (put-entry! bp3 0 11) => bp3
     (seq arr3) => (map cast [100 101 102])
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
