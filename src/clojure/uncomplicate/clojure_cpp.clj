;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-cpp
  "ClojureCPP is a Clojure library for integrating C/C++ based libraries available through JavaCPP.
  It is much more than a wrapper around JavaCPP that hides Java interop. It enables you to use JavaCPP's
  Pointer-based infrastucture the Clojure way; exposing what is absolutely necessary, automating whatever
  boilerplate that can be automated under the hood, and protecting yourself from shooting yourself
  in the foot with memory leaks and segmentation faults as much as possible (95%? 89%? who knows, but not 100%).
  You still have to be careful, because you're stepping outside the JVM, but you'll write a lot less code,
  your code will fit nicely with the rest of Clojure code in your program, and you will do the wrong
  thing less often.

  The center piece of this library is JavaCPP's `Pointer` class. Almost all JavaCPP interop methods
  accept subclasses of `Pointer` as arguments (along with good old Java primitives such as `int` or `double`).
  Very often, these pointers are pointers to off-heap primitive memory blocks that are not managed by
  your JVM, which are managed by classes such as `DoublePointer` or `FloatPointer`. That is because
  many C libraries use the respective arrays. Some libraries define their own structs; these are
  typically represented as specialized `Pointer` sublasses by JavaCPP. Typical examples would be
  CUDA or MKL wrappers. You can explore ClojureCUDA and Neanderthal code for real-world examples
  of how to deal with these if you need to create a new wrapper for a new library. Fortunately,
  if someone else has already done the integration, you might even write code without thinking
  much outside of Clojure. Depending of your grasp of the basics of C and/or C++, it still might be
  a good idea to read some [introduction to JavaCPP](https://github.com/bytedeco/javacpp).

  Note that ClojureCPP is well integrated into Clojure and Uncomplicate family of libraries,
  which means that lots of functionality is integrated into general functions such as
  `release`, `size`, `bytesize`, `sizeof` from `uncomplicate.commons`, or `fmap`, `fmap!`,
  `fold`, `op` from `uncomplicate.fluokitten`, or similar polymorphic functions. Ideally,
  you'll prefer these over fiddling with lower-level ClojureCPP-specific code (when possible).

  Here's a loose grouping of ClojureCPP functions:

  - System functions give you some insight int o the overall system, memory, and allocations:
  [[physical-bytes]], [[available-physical-bytes]], [[max-physical-bytes]], [[total-physical-bytes]],
  [[tracked-bytes]], [[max-tracked-bytes]] and [[pointers-count]].

  - Constructors. The pointers that you'll use throughout your code are explicitly created by these
  constructors. Alternatively, you'll work with pointers returned by custom functions from many of
  C libraries available through JavaCPP (such as CUDA etc.). You'll use these functions a lot.
  [[pointer]], [[pointer-pointer]], [[byte-pointer]], [[keyword-pointer]], [[string-pointer]],
  [[bool-pointer]], [[bool-pointer]], [[clong-pointer]], [[size-t-pointer]], [[char-pointer]],
  [[short-pointer]], [[int-pointer]], [[long-pointer]], [[float-pointer]], [[double-pointer]],
  and [[function-pointer]].

  - Memory allocation functions with standard C counterparts (you typically don't have to use these
  unless you're writing low-level stuff):
  [[malloc!]], [[calloc!]], [[realloc!]], [[free!]], [[memcmp]], [[memcpy!]], [[memmove!]],
  [[memset!]], and [[zero!]].

  - General pointer features:

  [[pointer-type]], [[pointer-class]], and [[type-pointer]] are convenience mappings between
  keywords representing primitive types, built-in pointer classes, and pointer constructor functions,
  so you can, for example, use `:float` or `Float` instead if importing `FloatPointer`. That also helps in
  integration with other systems without coupling with JavaCPP types.

  The following functions give you an insight into the properties of the pointer at hand. Most of
  the time, these properties are set by other functions to their right values, but sometimes you
  want to see details or even set something yourself (but be careful of papercuts!).
  [[address]], [[null?]], [[capacity]], [[capacity!]], [[limit]], [[limit!]], [[position]], [[position!]],

  - This group of functions do some pointer arithmetic, type casts, or type conversions:
  [[get-pointer]], [[safe]], [[safe2]], [[ptr*]], [[ptr]], [[ptr2]],
  [[float-ptr*]], [[float-ptr]], [[float-ptr2]], [[double-ptr*]], [[double-ptr]], [[double-ptr2]],
  [[long-ptr*]], [[long-ptr]], [[long-ptr2]], [[int-ptr*]], [[int-ptr]], [[int-ptr2]],
  [[short-ptr*]], [[short-ptr]], [[short-ptr2]], [[short-ptr*]], [[short-ptr]], [[short-ptr2]],
  [[byte-ptr*]], [[byte-ptr]], [[byte-ptr2]], [[size-t-ptr*]], [[size-t-ptr]], [[size-t-ptr2]],
  [[clong-ptr*]], [[clong-ptr]], [[clong-ptr2]], [[bool-ptr*]], [[bool-ptr]], [[bool-ptr2]],
  [[char-ptr*]], [[char-ptr]], and [[char-ptr2]].

  - Java and Clojure conversions to Java buffers or Clojure vectors and sequences:
  [[as-byte-buffer]], [[as-buffer]], [[pointer-vec]], [[pointer-seq]],

  - Polymorphic access to data in memory blocks managed by a pointer:
  [[get!]], [[put!]], [[get-entry!]], [[put-entry!]], and [[fill!]]

  - Raw access to bytes from a `BytePointer`:
  [[get-keyword]], [[put-keyword!]], [[get-string]], [[put-string!]],
  [[get-pointer-value]], [[put-pointer-value]], [[get-unsigned]], [[put-unsigned!]], [[get-bool]],
  [[put-bool!]], [[get-char]], [[put-char!]], [[get-int]], [[put-int!]], [[get-short]], [[put-short!]],
  [[get-long]], [[put-long!]], [[get-byte]], [[put-byte!]], [[get-short]], [[put-short!]],
  [[get-double]], [[put-double!]], [[get-float]], [[put-float!]], and [[get-string-bytes]].

  Please read [JavaCPP javadoc](http://bytedeco.org/javacpp/apidocs/overview-summary.html) for more internal details when necessary.
  Also, getting familiar with common C library functions can be very helpful.

  Please check out `uncomplicate.clojure-cpp-test` for examples of how to use these functions!"
  (:require [clojure.string :refer [split]]
            [uncomplicate.commons
             [core :refer [Releaseable let-release Info info Wrapper Wrappable Bytes Entries bytesize size]]
             [utils :refer [dragan-says-ex]]]
            [uncomplicate.fluokitten
             [core :as fluokitten]
             [protocols :refer [Functor PseudoFunctor Magma Foldable foldmap fold]]])
  (:import [java.nio Buffer ByteBuffer CharBuffer ShortBuffer IntBuffer LongBuffer FloatBuffer
            DoubleBuffer]
           java.nio.charset.Charset
           [clojure.lang Seqable Keyword IFn$LLL IFn$LDD IFn$LOO]
           [org.bytedeco.javacpp Pointer BytePointer CharPointer BoolPointer ShortPointer
            IntPointer LongPointer FloatPointer DoublePointer CLongPointer FunctionPointer
            PointerPointer SizeTPointer]
           [uncomplicate.clojure_cpp StringPointer KeywordPointer]))

;; ================= System =================================

(defn physical-bytes
  "Amount of non-shared physical memory currently used by the process.
  If provided with `max-size`, may return an approximate value, between real physical bytes and `max-size`,
  saving some processing time. Returns `0` if this amount can't be determined."
  (^long []
   (Pointer/physicalBytes))
  (^long [^long max-size]
   (Pointer/physicalBytesInaccurate max-size)))

(defn available-physical-bytes
  "Amount of physical memory that is available (free) from the operating system.
  Returns `0` if this amount can't be determined."
  ^long []
  (Pointer/availablePhysicalBytes))

(defn max-physical-bytes
  "The maximum amount of physical memory that should (could?) be used."
  ^long []
  (Pointer/maxPhysicalBytes))

(defn total-physical-bytes
  "The total amount of memory physically installed in the machine.
  The amount of `physical-bytes` can't be larger than this value.
  Returns `0` if the amount can't be determined."
  ^long []
  (Pointer/totalPhysicalBytes))

(defn tracked-bytes
  "The amount of memory currently tracked by JavaCPP deallocators."
  ^long []
  (Pointer/totalBytes))

(defn max-tracked-bytes
  "The maximum amount of memory allowed to be tracked by JavaCPP deallocators."
  ^long []
  (Pointer/maxBytes))

(defn pointers-count
  "Number of pointers currently tracked by JavaCPP deallocators."
  ^long []
  (Pointer/totalCount))

;; ================= A set of standard C functions =================================

(defn malloc!
  "Allocates the `byte-size` bytes of memory and returns a `Pointer` that manages it.
  The memory block allocated by `malloc!` has to be explicitly freed by `free!`.
  Calling `release` has no effect because no deallocator has been attached.
  It is very important to keep in mind that [[malloc!]] does NOT initialize the memory block.
  The danger is that often values could be `0`, and this may  trick you into believing that
  they typically will be initialized! In general, the memory block contains garbage, and must be
  explicitly initialized if your program relies on the values being `0` after allocation.
  If called with a negative number, returns `nil`.

  Prefer creating a pointer with (int-pointer 8) instead of with (int-pointer (malloc! 8)),
  unless you have a specific reason to do otherwise.
  This returns a fully configured, but still uninitialized, pointer that has a deallocator.
  "
  [^long byte-size]
  (when-let [p (Pointer/malloc byte-size)]
    (.capacity p byte-size)))

(defn calloc!
  "Allocated a memory block of `n` elements each taking `element-size` bytes, and initializes
  it to `0`. An alternative to [[malloc!]].

  (int-pointer (calloc! 2 8))
  => {:address \"0x7fd5b87596a0\", :type :int, :position 0, :limit 2, :capacity 2, :entries (0 0)}
  "
  [^long n ^long element-size]
  (when-let [p (Pointer/calloc n element-size)]
    (.capacity p (* n element-size))))

(defn realloc!
  "Attempts to resize a memory block previously created by `malloc!` or `calloc!`.
  "
  [^Pointer p ^long byte-size]
  (when-let [p (Pointer/realloc p byte-size)]
    (.capacity p byte-size)))

(defn free!
  "Deallocates the memory block that was allocated by [[calloc!]], [[malloc!]], or [[realloc!]].
  Although typically attempting to free a wrong block may hard crash your program,
  this function has protections against most of common errors, such as trying to free
  an already de-allocated block, or calling free after a careless pointer arithmetic.
  But there's always a way to shoot oneself in the foot, so please be careful with this.
  Returns a NULL pointer (not `nil`!).
  "
  [^Pointer p]
  (when-not (Pointer/isNull p)
    (Pointer/free (.position p 0))
    (.deallocate p)
    (.setNull p))
  p)

(defn memcmp
  "Compares the first `byte-size` bytes of `p1` and `p2`. Returns a `long` integer, not a boolean!
  The result is as follows:
  zero: `byte-size` bytes are equal
  negative: `p1` is less than `p2`
  positive: `p2` is less than `p1`

  If `byte-size` is not within bounds of `p1` and `p2`, throws `IndexOutOfBoundsException`.

  (memcmp (byte-pointer [1 2 3]) (byte-pointer [1 1 4]) 3) => 1
  "
  (^long [^Pointer p1 ^Pointer p2 ^long byte-size]
   (if (<= 0 byte-size (min (bytesize p1) (bytesize p2)))
     (Pointer/memcmp p1 p2 byte-size)
     (throw (IndexOutOfBoundsException.
             (format "You're trying to compare data outside the bounds of %s memory block."
                     (if (< (bytesize p1) (bytesize p2)) "p1" "p2"))))))
  (^long [^Pointer p1 ^Pointer p2]
   (Pointer/memcmp p1 p2 (min (bytesize p1) (bytesize p2)))))

(defn memcpy!
  "Copies `byte-size` bytes from `src` to `dst`, nad returns `dst`. If `byte-size` is not within
  bounds of `src` and `dst`, throws `IndexOutOfBoundsException`.
  "
  ([^Pointer src ^Pointer dst ^long byte-size]
   (if (<= 0 byte-size (min (bytesize src) (bytesize dst)))
     (Pointer/memcpy dst src byte-size)
     (throw (IndexOutOfBoundsException.
             (format "You're trying to copy outside the bounds of %s memory block."
                     (if (< (bytesize src) (bytesize dst)) "source" "destination"))))))
  ([^Pointer src ^Pointer dst]
   (Pointer/memcpy dst src (min (bytesize src) (bytesize dst)))))

(defn memmove!
  "Copies `byte-size` bytes from `src` to `dst`, and returns `dst`. A safer alternative
  to [[memcpy!]] in cases when `src` and `dst` contain overlapping memory blocks.
  If `byte-size` is not within bounds of `src` and `dst`, throws `IndexOutOfBoundsException`.
  "
  ([^Pointer src ^Pointer dst ^long byte-size]
   (if (<= 0 byte-size (min (bytesize src) (bytesize dst)))
     (Pointer/memmove dst src byte-size)
     (throw (IndexOutOfBoundsException.
             (format "You're trying to move data outside the bounds of %s memory block."
                     (if (< (bytesize src) (bytesize dst)) "source" "destination"))))))
  ([^Pointer src ^Pointer dst]
   (Pointer/memmove dst src (min (bytesize src) (bytesize dst)))))

(defn memset!
  "Sets `byte-size` bytes of `dst` to `value`, and returns `dst`.
  If `byte-size` is not within bounds of `src` and `dst`, throws `IndexOutOfBoundsException`.
  "
  [^Pointer dst ^long value ^long byte-size]
  (if (<= 0 byte-size (bytesize dst))
    (Pointer/memset dst value byte-size)
    (throw (IndexOutOfBoundsException. "You're trying to set data outside the bounds of the destination memory block."))))

(defn zero!
  "Initializes all elements in the memory block managed by `p` to zero."
  [^Pointer p]
  (.zero p))

;; ================= Pointer =================================

(def ^:const pointer-type
  "A mapping of JavaCPP pointer types to appropriate keywords.
  (pointer-type FloatPointer) => :float
  "
  {DoublePointer :double
   FloatPointer :float
   LongPointer :long
   IntPointer :int
   ShortPointer :short
   BytePointer :byte
   CharPointer :char
   CLongPointer :clong
   SizeTPointer :size-t
   BoolPointer :bool
   FunctionPointer :function
   PointerPointer :pointer
   Pointer :default})

(def ^:const pointer-class
  "A mapping of Java number types and related keywords to appropriate JavaCPP pointer types.
  (pointer-class :float) => FloatPointer
  "
  {:double DoublePointer
   :float FloatPointer
   :long LongPointer
   :int IntPointer
   :short ShortPointer
   :byte BytePointer
   :char CharPointer
   :clong CLongPointer
   :size-t SizeTPointer
   :bool BoolPointer
   :function FunctionPointer
   :fn FunctionPointer
   :pointer PointerPointer
   :default Pointer
   Double/TYPE DoublePointer
   Float/TYPE FloatPointer
   Long/TYPE LongPointer
   Integer/TYPE IntPointer
   Short/TYPE ShortPointer
   Byte/TYPE BytePointer
   Character/TYPE CharPointer
   Boolean/TYPE BoolPointer})

(defn address
  "Returns the address of pointer `p`"
  ^long [^Pointer p]
  (.address p))

(defn null?
  "Checks whether `p` points is `nil` or a NULL pointer. Typically, the NULL pointer
  is a pointer that has not yet been allocated (by [[malloc!]], [[calloc!]], or pointer constructors)
  or has been deallocated or freed.
  "
  [^Pointer p]
  (Pointer/isNull p))

(defn capacity
  "Returns the capacity of `p`, as number of bytes if `p` is just a `Pointer`, or in number
  of elements, if `p` is one of typed pointers such as `DoublePointer` or `IntPointer`.
  "
  ^long [^Pointer p]
  (.capacity p))

(defn capacity! ;;TODO think about removing this if Neanderthal and DD can use limit!
  "Sets the capacity of `p`, as number of bytes if `p` is just a `Pointer`, or in number
  of elements, if `p` is one of typed pointers such as `DoublePointer` or `IntPointer`.
  If `n` is negative, sets the capacity to `0`. Be warned that setting capacity to
  an arbitrarily large number can crash your program, or, worse, Heisenbugs.
  "
  [^Pointer p ^long n]
  (.capacity p (max 0 n)))

(defn limit
  "Returns the limit of `p`, as number of bytes if `p` is just a `Pointer`, or in number
  of elements, if `p` is one of typed pointers such as `DoublePointer` or `IntPointer`.
  "
  ^long [^Pointer p]
  (.limit p))

(defn limit!
  "Sets the limit of `p`, as number of bytes if `p` is just a `Pointer`, or in number
  of elements, if `p` is one of typed pointers such as `DoublePointer` or `IntPointer`.
  If `n` is negative, or larger than the available capacity, throws `IllegalArgumentexception`.
  "
  [^Pointer p ^long n]
  (if (<= 0 n (.capacity p))
    (.limit p n)
    (throw (IndexOutOfBoundsException. (format "The requested position %d is larger than capacity %d."
                                               n (.capacity p))))))

(defn position
  "Returns the position where `p` begins, as number of bytes if `p` is just a `Pointer`,
  or in number  of elements, if `p` is one of typed pointers such as `DoublePointer` or `IntPointer`.
  "
  ^long [^Pointer p]
  (.position p))

(defn position!
  "Sets the position where `p` begins, as number of bytes if `p` is just a `Pointer`,
  or in number of elements, if `p` is one of typed pointers such as `DoublePointer` or `IntPointer`.
  If `n` is negative, or larger than the available capacity, throws `IllegalArgumentexception`.
  "
  [^Pointer p ^long n]
  (if (<= 0 n (.capacity p))
    (.position p n)
    (throw (IndexOutOfBoundsException. (format "The requested position %d is larger than capacity %d."
                                               n (.capacity p))))))

(defn safe
  "If pointer `p` is neither `nil` nor `NULL`, returns `p`. Otherwise, throws an `IllegalArgumentexception`."
  ^Pointer [^Pointer p]
  (if-not (null? p)
    p
    (throw (IllegalArgumentException. "Neither nil nor NULL pointer is allowed in this part of code. Please do not use non-initialized pointers here."))))

(defn safe2
  "If pointer `p` is not `NULL`, returns `p`. Otherwise, throws an `IllegalArgumentexception`."
  ^Pointer [^Pointer p]
  (if (or (nil? p) (< 0 (.address p)))
    p
    (throw (IllegalArgumentException. "NULL pointer is not allowed in this part of code. Please do not use non-initialized pointers here."))))

(defn get-pointer
  "Returns a new pointer that manages the memory block managed by `p`, starting from element `i`,
  within bounds 0 and `(capacity p)`. If provided with pointer's type, coerces the pointer to it
  (please see [[pointer-class]] for available types).
  This is useful when you need to change some of pointer's properties in parts of code, but leave
  the original pointer unchanged.
  "
  (^Pointer [^Pointer p]
   (.getPointer p))
  (^Pointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p)))))
  (^Pointer [^Pointer p type ^long i]
   (.getPointer p (get pointer-class type type) (max 0 i))))

;; ================= Buffer =================================

(defn as-byte-buffer
  "Returns a `ByteBuffer` representation of a pointer."
  ^ByteBuffer [^Pointer p]
  (.asByteBuffer p))

(defn as-buffer ^Buffer [^Pointer p]
  (.asBuffer p))

(defprotocol ^:no-doc PointerCreator
  (pointer* [this] [this i] "Coerces a type into the appropriate `Pointer`."))

(defprotocol ^:no-doc RawPointerCreator
  (raw* [this] "Creates a new instance of this pointer's type with the same size."))

(defprotocol TypedPointerCreator
  (^PointerPointer pointer-pointer [this] [this charset] "Converts an object into `PointerPointer`.")
  (^BytePointer byte-pointer [this] [this charset] "Converts an object into `BytePointer`.")
  (^KeywordPointer keyword-pointer [this] [this charset] "Converts an object into `KeywordPointer`.")
  (^StringPointer string-pointer [this] [this charset] "Converts an object into `StringPointer`.")
  (^BoolPointer bool-pointer [this] "Converts an object into `BoolPointer`.")
  (^CLongPointer clong-pointer [this] "Converts an object into `CLongPointer`.")
  (^SizeTPointer size-t-pointer [this] "Converts an object into `SizeTPointer`.")
  (^CharPointer char-pointer [this] "Converts an object into `CharPointer`.")
  (^ShortPointer short-pointer [this] "Converts an object into `ShortPointer`.")
  (^IntPointer int-pointer [this] "Converts an object into `IntPointer`.")
  (^LongPointer long-pointer [this] "Converts an object into `LongPointer`.")
  (^FloatPointer float-pointer [this] "Converts an object into `FloatPointer`.")
  (^DoublePointer double-pointer [this] "Converts an object into `DoublePointer`.")
  (^FunctionPointer function-pointer [this] "Converts an object into `FunctionPointer`."))

(defprotocol PointerVec
  (pointer-vec [this]
    "Returns a vector representation of elements in pointer's memory block,
     taking into account pointer's type"))

(defn pointer
  "Coerces `x` to appropriate `Pointer` subclass, with position indicated by index `i`.
  The exact behavior is polymorphic per pointer type. If the argument is already a pointer,
  it just gets returned without change.

  Most common Clojure and Java collections are supported out of the box, with following
  characteristics regarding the new pointer's memory block:
  - The contents of a Java array is copied into new pointer's memory block,
    with no further connection between the two.
  - The contents of a `java.nio.Buffer` is copied into new pointer's memory block,
    with no further connection between the two.
  - The contents of a direct `java.nio.Buffer` is referenced by the pointer's memory,
    and each change is reflected in both.
  - The contents of a clojure sequence is copied into new pointer's memory block,
    with no further connection between the two.

  Custom classes are free to define they own way of converting to pointers by
  implementing the [[pointer*]] methods of the [[PointerCreator]] protocol.
  "
  (^Pointer [x]
   (pointer* x))
  (^Pointer [x i]
   (pointer* x i)))

(defn ptr*
  "Casts pointer `p` into `Pointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. If provided with index `i`,
  behaves like [[get-pointer]].
  "
  (^Pointer [^Pointer p]
   p)
  (^Pointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `Pointer`.
  Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros. If provided with index `i`, behaves like [[get-pointer]].
  "
  (^Pointer [^Pointer p]
   (safe p))
  (^Pointer [^Pointer p ^long i]
   (.getPointer (safe p) (max 0 (min i (.capacity p))))))

(defn ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `Pointer`.
  Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros. If provided with index `i`, behaves like [[get-pointer]].
  "
  (^Pointer [^Pointer p]
   (safe2 p))
  (^Pointer [^Pointer p ^long i]
   (.getPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn float-ptr*
  "Casts pointer `p` into `FloatPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `FloatPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[float-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^FloatPointer [p]
   p)
  (^FloatPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn float-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `FloatPointer`.
  Does not actually convert `p` into `FloatPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `FloatPointer`!.

  Prefer this method to [[float-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[float-ptr2]] in places when `nil` is not allowed.
  "
  (^FloatPointer [^FloatPointer p]
   (cast FloatPointer (safe p)))
  (^FloatPointer [^FloatPointer p ^long i]
   (.getPointer ^FloatPointer (safe p) (max 0 (min i (.capacity p))))))

(defn float-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `FloatPointer`.
  Does not actually convert `p` into `FloatPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `FloatPointer`!.

  Prefer this method to [[float-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[float-ptr]] in places when `nil` is acceptable.
  "
  (^FloatPointer [^FloatPointer p]
   (cast FloatPointer (safe2 p)))
  (^FloatPointer [^FloatPointer p ^long i]
   (.getPointer ^FloatPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn double-ptr*
  "Casts pointer `p` into `DoublePointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `DoublePointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[double-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^DoublePointer [^DoublePointer p]
   p)
  (^DoublePointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn double-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `DoublePointer`.
  Does not actually convert `p` into `DoublePointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `DoublePointer`!.

  Prefer this method to [[double-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[double-ptr2]] in places when `nil` is not allowed.
  "
  (^DoublePointer [^DoublePointer p]
   (cast DoublePointer (safe p)))
  (^DoublePointer [^DoublePointer p ^long i]
   (.getPointer ^DoublePointer (safe p) (max 0 (min i (.capacity p))))))

(defn double-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `DoublePointer`.
  Does not actually convert `p` into `DoublePointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `DoublePointer`!.

  Prefer this method to [[double-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[double-ptr]] in places when `nil` is acceptable.
  "
  (^DoublePointer [^DoublePointer p]
   (cast DoublePointer (safe2 p)))
  (^DoublePointer [^DoublePointer p ^long i]
   (.getPointer ^DoublePointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn long-ptr*
  "Casts pointer `p` into `LongPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `LongPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[long-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^LongPointer [^LongPointer p]
   p)
  (^LongPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn long-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `LongPointer`.
  Does not actually convert `p` into `LongPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `LongPointer`!.

  Prefer this method to [[long-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[long-ptr2]] in places when `nil` is not allowed.
  "
  (^LongPointer [^LongPointer p]
   (cast LongPointer (safe p)))
  (^LongPointer [^LongPointer p ^long i]
   (.getPointer ^LongPointer (safe p) (max 0 (min i (.capacity p))))))

(defn long-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `LongPointer`.
  Does not actually convert `p` into `LongPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `LongPointer`!.

  Prefer this method to [[long-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[long-ptr]] in places when `nil` is acceptable.
  "
  (^LongPointer [^LongPointer p]
   (cast LongPointer (safe2 p)))
  (^LongPointer [^LongPointer p ^long i]
   (.getPointer ^LongPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn int-ptr*
  "Casts pointer `p` into `IntPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `IntPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[int-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^IntPointer [^IntPointer p]
   p)
  (^IntPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn int-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `IntPointer`.
  Does not actually convert `p` into `IntPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `IntPointer`!.

  Prefer this method to [[int-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[int-ptr2]] in places when `nil` is not allowed.
  "
  (^IntPointer [^IntPointer p]
   (cast IntPointer (safe p)))
  (^IntPointer [^IntPointer p ^long i]
   (.getPointer ^IntPointer (safe p) (max 0 (min i (.capacity p))))))

(defn int-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `IntPointer`.
  Does not actually convert `p` into `IntPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `IntPointer`!.

  Prefer this method to [[int-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[int-ptr]] in places when `nil` is acceptable.
  "
  (^IntPointer [^IntPointer p]
   (cast IntPointer (safe2 p)))
  (^IntPointer [^IntPointer p ^long i]
   (.getPointer ^IntPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn short-ptr*
  "Casts pointer `p` into `ShortPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `ShortPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[short-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^ShortPointer [^ShortPointer p]
   p)
  (^ShortPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn short-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `ShortPointer`.
  Does not actually convert `p` into `ShortPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `ShortPointer`!.

  Prefer this method to [[short-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[short-ptr2]] in places when `nil` is not allowed.
  "
  (^ShortPointer [^ShortPointer p]
   (cast ShortPointer (safe p)))
  (^ShortPointer [^ShortPointer p ^long i]
   (.getPointer ^ShortPointer (safe p) (max 0 (min i (.capacity p))))))

(defn short-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `ShortPointer`.
  Does not actually convert `p` into `ShortPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `ShortPointer`!.

  Prefer this method to [[short-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[short-ptr]] in places when `nil` is acceptable.
  "
  (^ShortPointer [^ShortPointer p]
   (cast ShortPointer (safe2 p)))
  (^ShortPointer [^ShortPointer p ^long i]
   (.getPointer ^ShortPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn byte-ptr*
  "Casts pointer `p` into `BytePointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `BytePointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[byte-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^BytePointer [^BytePointer p]
   p)
  (^BytePointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn byte-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `BytePointer`.
  Does not actually convert `p` into `BytePointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `BytePointer`!.

  Prefer this method to [[byte-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[byte-ptr2]] in places when `nil` is not allowed.
  "
  (^BytePointer [^BytePointer p]
   (cast BytePointer (safe p)))
  (^BytePointer [^BytePointer p ^long i]
   (.getPointer ^BytePointer (safe p) (max 0 (min i (.capacity p))))))

(defn byte-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `BytePointer`.
  Does not actually convert `p` into `BytePointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `BytePointer`!.

  Prefer this method to [[byte-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[byte-ptr]] in places when `nil` is acceptable.
  "
  (^BytePointer [^BytePointer p]
   (cast BytePointer (safe2 p)))
  (^BytePointer [^BytePointer p ^long i]
   (.getPointer ^BytePointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn clong-ptr*
  "Casts pointer `p` into `CLongPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `CLongPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[clong-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^CLongPointer [^CLongPointer p]
   p)
  (^CLongPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn clong-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `CLongPointer`.
  Does not actually convert `p` into `CLongPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `CLongPointer`!.

  Prefer this method to [[clong-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[clong-ptr2]] in places when `nil` is not allowed.
  "
  (^CLongPointer [^CLongPointer p]
   (cast CLongPointer (safe p)))
  (^CLongPointer [^CLongPointer p ^long i]
   (.getPointer ^CLongPointer (safe p) (max 0 (min i (.capacity p))))))

(defn clong-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `CLongPointer`.
  Does not actually convert `p` into `CLongPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `CLongPointer`!.

  Prefer this method to [[clong-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[clong-ptr]] in places when `nil` is acceptable.
  "
  (^CLongPointer [^CLongPointer p]
   (cast CLongPointer (safe2 p)))
  (^CLongPointer [^CLongPointer p ^long i]
   (.getPointer ^CLongPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn size-t-ptr*
  "Casts pointer `p` into `SizeTPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `SizeTPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[size-t-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^SizeTPointer [^SizeTPointer p]
   p)
  (^SizeTPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn size-t-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `SizeTPointer`.
  Does not actually convert `p` into `SizeTPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually convert `p`
  into `SizeTPointer`!.

  Prefer this method to [[size-t-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[size-t-ptr2]] in places when `nil` is not allowed.
  "
  (^SizeTPointer [^SizeTPointer p]
   (cast SizeTPointer (safe p)))
  (^SizeTPointer [^SizeTPointer p ^long i]
   (.getPointer ^SizeTPointer (safe p) (max 0 (min i (.capacity p))))))

(defn size-t-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `SizeTPointer`.
  Does not actually convert `p` into `SizeTPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `SizeTPointer`!.

  Prefer this method to [[size-t-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[size-t-ptr]] in places when `nil` is acceptable.
  "
  (^SizeTPointer [^SizeTPointer p]
   (cast SizeTPointer (safe2 p)))
  (^SizeTPointer [^SizeTPointer p ^long i]
   (.getPointer ^SizeTPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn bool-ptr*
  "Casts pointer `p` into `BoolPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `BoolPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[bool-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^BoolPointer [^BoolPointer p]
   p)
  (^BoolPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn bool-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `BoolPointer`.
  Does not actually convert `p` into `BoolPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `BoolPointer`!.

  Prefer this method to [[bool-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[bool-ptr2]] in places when `nil` is not allowed.
  "
  (^BoolPointer [^BoolPointer p]
   (cast BoolPointer (safe p)))
  (^BoolPointer [^BoolPointer p ^long i]
   (.getPointer ^BoolPointer (safe p) (max 0 (min i (.capacity p))))))

(defn bool-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `BoolPointer`.
  Does not actually convert `p` into `BoolPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `BoolPointer`!.

  Prefer this method to [[bool-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[bool-ptr]] in places when `nil` is acceptable.
  "
  (^BoolPointer [^BoolPointer p]
   (cast BoolPointer (safe2 p)))
  (^BoolPointer [^BoolPointer p ^long i]
   (.getPointer ^BoolPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defn char-ptr*
  "Casts pointer `p` into `CharPointer`. Useful when metadata for avoiding reflection
  is not easily added in code, such as in macros. Does not actually convert `p` into `CharPointer`!.
  It just does the type cast to satisfy Clojure's compiler.

  If provided with index `i`, behaves like [[get-pointer]].

  Prefer this method to [[char-ptr]] in places when you only care about satisfying the compiler,
  and don't care about NULL pointers.
  "
  (^CharPointer [^CharPointer p]
   p)
  (^CharPointer [^Pointer p ^long i]
   (.getPointer p (max 0 (min i (.capacity p))))))

(defn char-ptr
  "Checks pointer `p` for safety with [[safe]] and casts it into `CharPointer`.
  Does not actually convert `p` into `CharPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `CharPointer`!.

  Prefer this method to [[char-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[char-ptr2]] in places when `nil` is not allowed.
  "
  (^CharPointer [^CharPointer p]
   (cast CharPointer (safe p)))
  (^CharPointer [^CharPointer p ^long i]
   (.getPointer ^CharPointer (safe p) (max 0 (min i (.capacity p))))))

(defn char-ptr2
  "Checks pointer `p` for safety with [[safe2]] and casts it into `CharPointer`.
  Does not actually convert `p` into `CharPointer`!. It just does the type cast to satisfy
  Clojure's compiler. Useful when metadata for avoiding reflection is not easily added in code,
  such as in macros.

  If provided with index `i`, behaves like [[get-pointer]]. It DOES actually require `p`
  to be a `CharPointer`!.

  Prefer this method to [[char-ptr*]] in places when NULL pointer can cause harm.
  Prefer this method to [[char-ptr]] in places when `nil` is acceptable.
  "
  (^CharPointer [^CharPointer p]
   (cast CharPointer (safe2 p)))
  (^CharPointer [^CharPointer p ^long i]
   (.getPointer ^CharPointer (safe2 p) (max 0 (min i (.capacity p))))))

(defprotocol Accessor
  (get! [pointer dst!] [pointer dst! offset length] "Copies data from pointer's memory block into `dst`, which is typically a Java array.")
  (put! [pointer! src] [pointer! src offset length] "Copies data from a Java array or a Clojure sequence `src` to  this pointer's memory block.")
  (get-entry [pointer] [pointer i] "Gets the value at index `i` in pointer's memory block.")
  (put-entry! [pointer value] [pointer i value] "Puts value into pointer's memory block at index `i`.")
  (fill! [pointer value] "Sets all elements in pointer's memory block to `value`."))

(defprotocol ^:no-doc PutEntry
  (put-entry* [value pointer] [value i pointer] "A convenience method to facilitate BytePointer's put-entry!."))

(defprotocol ^:no-doc PutPointer
  (put-pointer-pointer* [src dst] [arg src dst] "A convenience method to facilitate PointerPointer's put!."))

(defn type-pointer
  "Returns the appropriate constructor for the pointer of type `t`, such as [[float-pointer]]
  for `:float` or `float`."
  [t]
  (case t
    :float float-pointer
    :double double-pointer
    :long long-pointer
    :int int-pointer
    :short short-pointer
    :byte byte-pointer
    :char char-pointer
    :size-t size-t-pointer
    :clong clong-pointer
    :pointer pointer-pointer
    :bool bool-pointer
    :function function-pointer
    :uint8 byte-pointer
    Float/TYPE float-pointer
    Double/TYPE double-pointer
    Long/TYPE long-pointer
    Integer/TYPE int-pointer
    Short/TYPE short-pointer
    Byte/TYPE byte-pointer
    Character/TYPE char-pointer
    Boolean/TYPE bool-pointer
    nil))

(let [get-deallocator (doto (.getDeclaredMethod Pointer "deallocator" (make-array Class 0))
                        (.setAccessible true))
      empty-array (into-array [])]
  (extend-type Pointer
    Releaseable
    (release [this]
      (.deallocate this)
      true)
    Info
    (info
      ([this]
       {:address (.address this)
        :type (let [t (type this)] (get pointer-type t t))
        :position (.position this)
        :limit (.limit this)
        :capacity (.capacity this)
        :deallocator (.invoke get-deallocator this empty-array)})
      ([this info-type]
       (case info-type
         :address (.address this)
         :type (let [t (type this)] (get pointer-type t t))
         :position (.position this)
         :limit (.limit this)
         :capacity (.capacity this)
         :deallocator (.invoke get-deallocator this empty-array)
         nil)))
    Wrapper
    (extract [this]
      (if-not (null? this) this nil))
    Wrappable
    (wrap [this]
      this)
    Bytes
    (bytesize* [this]
      (max 0 (* (.sizeof this) (- (.limit this) (.position this)))))
    Entries
    (sizeof* [this]
      (.sizeof this))
    (size* [this]
      (max 0 (- (.limit this) (.position this))))
    PointerCreator
    (pointer*
      ([this]
       this)
      ([this i]
       (.getPointer this ^long i)))
    TypedPointerCreator
    (byte-pointer [this]
      (.getPointer this BytePointer 0))
    (keyword-pointer [this]
      (.getPointer this KeywordPointer 0))
    (string-pointer [this]
      (.getPointer this StringPointer 0))
    (clong-pointer [this]
      (.getPointer this CLongPointer 0))
    (size-t-pointer [this]
      (.getPointer this SizeTPointer 0))
    (bool-pointer [this]
      (.getPointer this BoolPointer 0))
    (pointer-pointer [this]
      (.getPointer this PointerPointer 0))
    (char-pointer [this]
      (.getPointer this CharPointer 0))
    (short-pointer [this]
      (.getPointer this ShortPointer 0))
    (int-pointer [this]
      (.getPointer this IntPointer 0))
    (long-pointer [this]
      (.getPointer this LongPointer 0))
    (float-pointer [this]
      (.getPointer this FloatPointer 0))
    (double-pointer [this]
      (.getPointer this DoublePointer 0))
    (function-pointer [this]
      (.getPointer this FunctionPointer 0))))

(extend-type nil
  PointerCreator
  (pointer*
    ([_]
     (Pointer.))
    ([_ _]
     (Pointer.)))
  RawPointerCreator
  (raw* [_]
    (Pointer.))
  TypedPointerCreator
  (byte-pointer [_]
    (BytePointer.))
  (string-pointer [_]
    (StringPointer.))
  (clong-pointer [_]
    (CLongPointer.))
  (size-t-pointer [_]
    (SizeTPointer.))
  (bool-pointer [_]
    (BoolPointer.))
  (pointer-pointer [_]
    (PointerPointer.))
  (char-pointer [_]
    (CharPointer.))
  (short-pointer [_]
    (ShortPointer.))
  (int-pointer [_]
    (IntPointer.))
  (long-pointer [_]
    (LongPointer.))
  (float-pointer [_]
    (FloatPointer.))
  (double-pointer [_]
    (DoublePointer.)))

(defmacro ^:private create-new*
  ([constructor size]
   `(if (<= 0 (long ~size))
      (new ~constructor (long ~size))
      (dragan-says-ex "Array size must be 0 or larger." {:size ~size})))
  ([constructor arg-type arg]
   `(new ~constructor ~(with-meta arg {:tag arg-type}))))

(defmacro ^:private extend-creator [ct]
  `(extend-type ~ct
     TypedPointerCreator
     (byte-pointer [this#]
       (create-new* BytePointer this#))
     (keywrod-pointer [this#]
       (create-new* KeywordPointer this#))
     (string-pointer [this#]
       (create-new* StringPointer this#))
     (clong-pointer [this#]
       (create-new* CLongPointer this#))
     (size-t-pointer [this#]
       (create-new* SizeTPointer this#))
     (bool-pointer [this#]
       (create-new* BoolPointer this#))
     (pointer-pointer [this#]
       (create-new* PointerPointer this#))
     (char-pointer [this#]
       (create-new* CharPointer this#))
     (short-pointer [this#]
       (create-new* ShortPointer this#))
     (int-pointer [this#]
       (create-new* IntPointer this#))
     (long-pointer [this#]
       (create-new* LongPointer this#))
     (float-pointer [this#]
       (create-new* FloatPointer this#))
     (double-pointer [this#]
       (create-new* DoublePointer this#))))

(extend-creator Character)
(extend-creator Byte)
(extend-creator Short)
(extend-creator Integer)
(extend-creator Long)
(extend-creator Boolean)

(defmacro ^:private extend-number [nt pt]
  `(extend-type ~nt
     PointerCreator
     (pointer*
       ([this#]
        (.put (new ~pt 1) this#))
       ([this# _#]
        (.put (new ~pt 1) this#)))))

(extend-number Double DoublePointer)
(extend-number Float FloatPointer)
(extend-number Long LongPointer)
(extend-number Integer IntPointer)
(extend-number Short ShortPointer)
(extend-number Byte BytePointer)
(extend-number Character CharPointer)

(defmacro ^:private get* [class p i]
  `(.get ~(with-meta p {:tag class}) ~i))

(defmacro ^:private put* [class p i e]
  `(.put ~(with-meta p {:tag class}) ~i ~e))

(defmacro ^:private from-seqable [class cast]
  `(fn [s#]
     (let-release [res# (new ~class (count s#))]
       (reduce (fn [i# e#]
                 (put* ~class res# i# (~cast e#))
                 (inc (long i#)))
               0
               s#)
       res#)))

(extend Seqable
  TypedPointerCreator
  {:byte-pointer (from-seqable BytePointer byte)
   :clong-pointer (from-seqable CLongPointer long)
   :size-t-pointer (from-seqable SizeTPointer long)
   :bool-pointer (from-seqable BoolPointer boolean)
   :pointer-pointer (from-seqable PointerPointer pointer)
   :char-pointer (from-seqable CharPointer char)
   :short-pointer (from-seqable ShortPointer short)
   :int-pointer (from-seqable IntPointer int)
   :long-pointer (from-seqable LongPointer long)
   :float-pointer (from-seqable FloatPointer float)
   :double-pointer (from-seqable DoublePointer double)})

(extend-type Buffer
  PointerCreator
  (pointer*
    ([b]
     (Pointer. b))
    ([b i]
     (.position (Pointer. b) i))))

(extend-type ByteBuffer
  PointerCreator
  (pointer*
    ([this]
     (BytePointer. this))
    ([this i]
     (.position (BytePointer. this) (long i))))
  TypedPointerCreator
  (byte-pointer [this]
    (BytePointer. this))
  (string-pointer [this]
    (StringPointer. this))
  (char-pointer [this]
    (CharPointer. (.asCharBuffer this)))
  (short-pointer [this]
    (ShortPointer. (.asShortBuffer this)))
  (int-pointer [this]
    (IntPointer. (.asIntBuffer this)))
  (long-pointer [this]
    (LongPointer. (.asLongBuffer this)))
  (float-pointer [this]
    (FloatPointer. (.asFloatBuffer this)))
  (double-pointer [this]
    (DoublePointer. (.asDoubleBuffer this))))

(defmacro ^:private extend-buffer [buffer-class pt method]
  `(extend-type ~buffer-class
     PointerCreator
     (pointer*
       ([this#]
        (create-new* ~pt ~buffer-class this#))
       ([this# i#]
        (position! (create-new* ~pt ~buffer-class this#) i#)))
     TypedPointerCreator
     (~method [this#]
      (create-new* ~pt ~buffer-class this#))))

(extend-buffer CharBuffer CharPointer char-pointer)
(extend-buffer ShortBuffer ShortPointer short-pointer)
(extend-buffer IntBuffer IntPointer int-pointer)
(extend-buffer LongBuffer LongPointer long-pointer)
(extend-buffer FloatBuffer FloatPointer float-pointer)
(extend-buffer DoubleBuffer DoublePointer double-pointer)

(defmacro ^:private access*
  ([method pt dst src]
   `(. ~(with-meta dst {:tag pt}) ~method ~(with-meta src {:tag Pointer})))
  ([method pt p val-type a]
   `(. ~(with-meta p {:tag pt}) ~method ~(with-meta a {:tag val-type})))
  ([method pt p val-type a offset length]
   `(. ~(with-meta p {:tag pt}) ~method ~(with-meta a {:tag val-type}) ~offset ~length)))

(defmacro ^:private extend-array [array-class array-type pt method]
  `(extend-type ~array-class
     PointerCreator
     (pointer*
       ([this#]
        (create-new* ~pt ~array-type this#))
       ([this# i#]
        (position! (create-new* ~pt ~array-type this#) i#)))
     TypedPointerCreator
     (~method [this#]
      (create-new* ~pt ~array-type this#))
     PutEntry
     (put-entry*
       ([this# p#]
        (access* put ~pt p# ~array-type this#)))))

(extend-array (Class/forName "[F") floats FloatPointer float-pointer)
(extend-array (Class/forName "[D") doubles DoublePointer double-pointer)
(extend-array (Class/forName "[C") chars CharPointer char-pointer)
(extend-array (Class/forName "[B") bytes BytePointer byte-pointer)
(extend-array (Class/forName "[S") shorts ShortPointer short-pointer)
(extend-array (Class/forName "[I") ints IntPointer int-pointer)
(extend-array (Class/forName "[J") longs LongPointer long-pointer)

(extend-type (Class/forName "[J")
  TypedPointerCreator
  (clong-pointer [this]
    (CLongPointer. ^longs this))
  (size-t-pointer [this]
    (SizeTPointer. ^longs this))
  (long-pointer [this]
    (LongPointer. ^longs this)))

(extend-type (Class/forName "[[B")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[B" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[B" this)))

(extend-type (Class/forName "[[S")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[S" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[S" this)))

(extend-type (Class/forName "[[I")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[I" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[I" this)))

(extend-type (Class/forName "[[J")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[J" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[J" this)))

(extend-type (Class/forName "[[F")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[F" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[F" this)))

(extend-type (Class/forName "[[D")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[D" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[D" this)))

(extend-type (Class/forName "[[C")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[C" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[[C" this)))

(extend-type (Class/forName "[Lorg.bytedeco.javacpp.Pointer;")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[Lorg.bytedeco.javacpp.Pointer;" this))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.put ^PointerPointer pp ^"[Lorg.bytedeco.javacpp.Pointer;" this)))

(extend-type (Class/forName "[Ljava.lang.String;")
  TypedPointerCreator
  (pointer-pointer
    ([this]
     (PointerPointer. ^"[Ljava.lang.String;" this))
    ([this charset]
     (if (string? charset)
       (PointerPointer. ^"[Ljava.lang.String;" this ^String charset)
       (PointerPointer. ^"[Ljava.lang.String;" this ^Charset charset))))
  PutPointer
  (put-pointer-pointer* [this pp]
    (.putString ^PointerPointer pp ^"[Ljava.lang.String;" this)))

(defn ^:private keyword-pointer*
  (^KeywordPointer [k]
   (KeywordPointer. (subs (str k) 1)))
  (^KeywordPointer [k charset]
   (if (string? charset)
     (KeywordPointer. (subs (str k) 1) ^String charset)
     (KeywordPointer. (subs (str k) 1) ^Charset charset))))

(defn get-keyword
  "Converts a `BytePointer's` memory block to keyword. "
  (^Keyword [^BytePointer p]
   (apply keyword (split (.getString p) #"/")))
  (^Keyword [^BytePointer p charset]
   (apply keyword (split (if (string? charset)
                                          (.getString p ^String charset)
                                          (.getString p ^Charset charset)) #"/"))))


(defn put-keyword!
  "Puts keyword value `k` using `charset` in `BytePointer's` memory block."
  [^BytePointer p ^Keyword k charset]
  (if (string? charset)
    (.putString p (subs (str k) 1) ^String charset)
    (.putString p (subs (str k) 1) ^Charset charset))
  p)

(extend-type Keyword
  PointerCreator
  (pointer*
    ([k]
     (keyword-pointer* k))
    ([k i]
     (.position (keyword-pointer* k) (long i))))
  TypedPointerCreator
  (byte-pointer
    ([k]
     (BytePointer. (subs (str k) 1)))
    ([k charset]
     (if (string? charset)
       (BytePointer. (subs (str k) 1) ^String charset)
       (BytePointer. (subs (str k) 1) ^Charset charset))))
  (byte-pointer
    ([k]
     (StringPointer. (subs (str k) 1)))
    ([k charset]
     (if (string? charset)
       (StringPointer. (subs (str k) 1) ^String charset)
       (StringPointer. (subs (str k) 1) ^Charset charset))))
  (keyword-pointer
    ([k]
     (keyword-pointer* k))
    ([k charset]
     (keyword-pointer* k charset)))
  PutPointer
  (put-pointer-pointer* [charset-name src pp]
    (.putString ^PointerPointer pp
                ^"[Ljava.lang.String;" (fluokitten/fmap! #(keyword-pointer* %) src)
                ^String charset-name)))

(defn get-string
  "Converts a `BytePointer's` memory block to string."
  (^String [^BytePointer p]
   (.getString p))
  (^String [^BytePointer p charset]
   (if (string? charset)
     (.getString p ^String charset)
     (.getString p ^Charset charset))))

(defn put-string!
  "Puts string value `s` using `charset` in `BytePointer's` memory block."
  [^BytePointer p ^String s charset]
  (if (string? charset)
    (.putString p s ^String charset)
    (.putString p s ^Charset charset))
  p)

(extend-type String
  PointerCreator
  (pointer*
    ([s]
     (StringPointer. s))
    ([s i]
     (.position (StringPointer. s) (long i))))
  TypedPointerCreator
  (byte-pointer
    ([s]
     (BytePointer. s))
    ([s charset]
     (if (string? charset)
       (BytePointer. s ^String charset)
       (BytePointer. s ^Charset charset))))
  (string-pointer
    ([s]
     (StringPointer. s))
    ([s charset]
     (if (string? charset)
       (StringPointer. s ^String charset)
       (StringPointer. s ^Charset charset))))
  (keyword-pointer
    ([s]
     (KeywordPointer. s))
    ([s charset]
     (if (string? charset)
       (KeywordPointer. s ^String charset)
       (KeywordPointer. s ^Charset charset))))
  PutPointer
  (put-pointer-pointer* [charset-name src pp]
    (.putString ^PointerPointer pp ^"[Ljava.lang.String;" src ^String charset-name)))

(extend-type Charset
  PutPointer
  (put-pointer-pointer* [charset src pp]
    (.putString ^PointerPointer pp ^"[Ljava.lang.String;" src ^String charset)))

(defn get-pointer-value
  "Gets a `Pointer` as the value at index `i` in `BytePointer's` memory block."
  ([^BytePointer p]
   (.getPointerValue p))
  ([^BytePointer p ^long i]
   (.getPointerValue p i)))

(defn put-pointer-value!
  "Puts `Pointer ``x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^Pointer x]
  (.putPointerValue p i x)
  p)

(defn get-unsigned
  "Gets a `Pointer` as the value at index `i` in `BytePointer's` memory block."
  (^long [^BytePointer p]
   (.getUnsigned p))
  (^long [^BytePointer p ^long i]
   (.getUnsigned p i)))

(defn put-unsigned!
  "Puts unsigned long value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^long x]
  (.putUnsigned p i x)
  p)

(defn get-bool
  "Gets the bool value at index `i` in `BytePointer's` memory block."
  ([^BytePointer p]
   (.getBool p))
  ([^BytePointer p ^long i]
   (.getBool p i)))

(defn put-bool!
  "Puts bool value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^Boolean x]
  (.putBool p i x)
  p)

(defn get-char
  "Gets the char value at index `i` in `BytePointer's` memory block."
  ([^BytePointer p]
   (.getChar p))
  ([^BytePointer p ^long i]
   (.getChar p i)))

(defn put-char!
  "Puts char value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i x]
  (.putChar p i x)
  p)

(defn get-int
  "Gets the integer value at index `i` in `BytePointer's` memory block."
  (^long [^BytePointer p]
   (.getInt p))
  (^long [^BytePointer p ^long i]
   (.getInt p (* Integer/BYTES i))))

(defn put-int!
  "Puts char value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^long x]
  (.putInt p (* Integer/BYTES i) x)
  p)

(defn get-long
  "Gets the long value at index `i` in `BytePointer's` memory block."
  (^long [^BytePointer p]
   (.getLong p))
  (^long [^BytePointer p ^long i]
   (.getLong p (* Long/BYTES i))))

(defn put-long!
  "Puts long value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^long x]
  (.putLong p (* Long/BYTES i) x)
  p)

(defn get-byte
  "Gets the byte value at index `i` in `BytePointer's` memory block."
  (^long [^BytePointer p]
   (long (.get p)))
  (^long [^BytePointer p ^long i]
   (long (.get p (* Byte/BYTES i)))))

(defn put-byte!
  "Puts byte value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^long x]
  (.put p i x)
  p)

(defn get-short
  "Gets the short value at index `i` in `BytePointer's` memory block."
  (^long [^BytePointer p]
   (long (.getShort p)))
  (^long [^BytePointer p ^long i]
   (long (.getShort p (* Short/BYTES i)))))

(defn put-short!
  "Puts short value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^long x]
  (.putShort p (* Short/BYTES i) x)
  p)

(defn get-double
  "Gets the double value at index `i` in `BytePointer's` memory block."
  (^double [^BytePointer p]
   (.getDouble p))
  (^double [^BytePointer p ^long i]
   (.getDouble p (* Double/BYTES i))))

(defn put-double!
  "Puts double value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^double x]
  (.putDouble p (* Double/BYTES i) x)
  p)

(defn get-float
  "Gets the float value at index `i` in `BytePointer's` memory block."
  (^double [^BytePointer p]
   (.getFloat p))
  (^double [^BytePointer p ^long i]
   (.getFloat p (* Float/BYTES i))))

(defn put-float!
  "Puts float value `x` at index `i` in `BytePointer's` memory block."
  [^BytePointer p ^long i ^double x]
  (.putFloat p (* Float/BYTES i) x)
  p)

(defn get-string-bytes
  "Assuming that `p` contains a null-terminated string, returns its byte
  representation in a byte array."
  ^bytes [^BytePointer p]
  (.getStringBytes p))

(defn pointer-seq
  "Creates a lazy seq of this pointer's elements. Similar to `clojure.core/seq`. "
  [^Pointer p]
  (letfn [(pointer-seq* [p ^long i ^long n]
            (lazy-seq
             (if (< -1 i n)
               (cons (get-entry p i) (pointer-seq* p (inc i) n))
               '())))]
    (if (null? p)
      nil
      (pointer-seq* p 0 (size p)))))

(defmacro ^:private extend-pointer [pt zero? entry-type array-type convert-fn]
  `(extend-type ~pt
     PointerCreator
     (pointer*
       ([this#]
        this#)
       ([this# i#]
        (get-pointer this# i#)))
     RawPointerCreator
     (raw* [this#]
       (new ~pt (size this#)))
     Accessor
     (get-entry
       ([this#]
        (.get this#))
       ([this# i#]
        (.get this# (long i#))))
     (put-entry!
       ([this# value#]
        (.put this# (~entry-type value#))
        this#)
       ([this# i# value#]
        (.put this# i# (~entry-type value#))
        this#))
     (fill! [this# value#]
       (let [v# (~entry-type value#)]
         (if (~zero? v#)
           (.zero this#)
           (dotimes [i# (size this#)]
             (.put this# i# (~entry-type value#)))))
       this#)
     (get!
       ([p# arr#]
        (access* get ~pt p# ~array-type arr#)
        arr#)
       ([p# arr# offset# length#]
        (access* get ~pt p# ~array-type arr# offset# length#)
        arr#))
     (put!
       ([p# obj#]
        (if (sequential? obj#)
          (.put p# (~array-type (~convert-fn obj#)))
          (.put p# (~array-type obj#)))
        p#)
       ([p# obj# offset# length#]
        (if (sequential? obj#)
          (.put p# (~array-type (~convert-fn (take length# (drop offset# obj#)))))
          (.put p# obj# offset# length#))))
     PointerVec
     (pointer-vec [this#]
       (let [n# (size this#)]
         (loop [res# (transient []) i# 0]
           (if (< i# n#)
             (recur (conj! res# (.get this# i#)) (inc i#))
             (persistent! res#)))))))

(extend-pointer BytePointer zero? byte bytes byte-array)
(extend-pointer CLongPointer zero? long longs long-array)
(extend-pointer SizeTPointer zero? long longs long-array)
(extend-pointer CharPointer (fn [c] (= \  (char c))) char chars #(char-array (map char %)))
(extend-pointer ShortPointer zero? short shorts short-array)
(extend-pointer IntPointer zero? int ints int-array)
(extend-pointer LongPointer zero? long longs long-array)
(extend-pointer FloatPointer zero? float floats float-array)
(extend-pointer DoublePointer zero? double doubles double-array)

(extend-type BytePointer
  Accessor
  (get-entry
    ([this]
     (.get this))
    ([this i]
     (.get this (long i))))
  (put-entry!
    ([this value]
     (put-entry* value this)
     this)
    ([this i value]
     (put-entry* value i this)
     this))
  (fill! [this value]
    (if (zero? (byte value))
      (.zero this)
      (.fill this value))
    this)
  (get!
    ([p arr]
     (.get p (bytes arr))
     arr)
    ([p arr offset length]
     (.get p (bytes arr) offset length)
     arr))
  (put!
    ([p obj]
     (if (sequential? obj)
       (.put p (byte-array obj))
       (.put p (bytes obj)))
     p)
    ([p obj offset length]
     (if (sequential? obj)
       (.put p (byte-array (take length (drop offset obj))))
       (.put p obj offset length)))))

(extend-type BoolPointer
  PointerCreator
  (pointer*
    ([this]
     this)
    ([this i]
     (get-pointer this i)))
  RawPointerCreator
  (raw* [this]
    (BoolPointer. (size this)))
  Accessor
  (get-entry
    ([this]
     (.get this))
    ([this i]
     (.get this (long i))))
  (put-entry!
    ([this value]
     (.put this (boolean value))
     this)
    ([this i value]
     (.put this i (boolean value))
     this))
  (fill! [this value]
    (dotimes [i (size this)]
      (.put this i (boolean value)))
    this)
  PointerVec
  (pointer-vec [this]
    (let [n (size this)]
      (loop [res (transient []) i 0]
        (if (< i n)
          (recur (conj! res (.get this i)) (inc i))
          (persistent! res))))))

(defmacro ^:private extend-entry [number-type put-method]
  `(extend-type ~number-type
     PutEntry
     (put-entry*
       ([this# p#]
        (~put-method p# 0 this#))
       ([this# i# p#]
        (~put-method p# i# this#)))))

(extend-entry Double put-double!)
(extend-entry Float put-float!)
(extend-entry Long put-long!)
(extend-entry Integer put-int!)
(extend-entry Short put-short!)
(extend-entry Boolean put-bool!)
(extend-entry Byte put-byte!)
(extend-entry Character put-char!)
(extend-entry Pointer put-pointer-value!)

(extend-type PointerPointer
  PointerCreator
  (pointer*
    ([this]
     this)
    ([this i]
     (get-pointer this i)))
  RawPointerCreator
  (raw* [this]
    (PointerPointer. (size this)))
  Accessor
  (get-entry
    ([this]
     (.get this))
    ([this i]
     (.get this (long i))))
  (put-entry!
    ([this value]
     (.put this ^Pointer value)
     this)
    ([this i value]
     (.put this i ^Pointer value)
     this))
  (fill! [this value]
    (dotimes [i (size this)]
      (.put this i ^Pointer value))
    this)
  (put!
    ([p obj]
     (put-pointer-pointer* obj p)
     p)
    ([this src arg]
     (put-pointer-pointer* arg src this)
     this))
  PointerVec
  (pointer-vec [this#]
    (let [n# (size this#)]
      (loop [res# (transient []) i# 0]
        (if (< i# n#)
          (recur (conj! res# (.get this# i#)) (inc i#))
          (persistent! res#))))))

(extend-type StringPointer
  PointerCreator
  (pointer*
    ([this]
     this))
  RawPointerCreator
  (raw* [this]
    (StringPointer. (get-string this)))
  Wrapper
  (extract [this]
    (get-string this)))

(extend-type KeywordPointer
  PointerCreator
  (pointer*
    ([this]
     this))
  RawPointerCreator
  (raw* [this]
    (KeywordPointer. (get-keyword this)))
  Wrapper
  (extract [this]
    (get-keyword this)))

(defn ^:private write-pointer [p ^java.io.Writer w]
  (.write w (pr-str (-> (info p)
                        (dissoc :deallocator)
                        (assoc :entries (pointer-seq p))
                        (update :address (partial format "0x%x"))))))

(defmethod print-method Pointer
  [p ^java.io.Writer w]
  (.write w (pr-str (update (info p) :address (partial format "0x%x")))))

(defmethod print-method FloatPointer
  [p w]
  (write-pointer p w))

(defmethod print-method DoublePointer
  [p w]
  (write-pointer p w))

(defmethod print-method IntPointer
  [p w]
  (write-pointer p w))

(defmethod print-method LongPointer
  [p w]
  (write-pointer p w))

(defmethod print-method ShortPointer
  [p w]
  (write-pointer p w))

(defmethod print-method BytePointer
  [p w]
  (write-pointer p w))

(defmethod print-method SizeTPointer
  [p w]
  (write-pointer p w))

(defmethod print-method CLongPointer
  [p w]
  (write-pointer p w))

(defmethod print-method BoolPointer
  [p w]
  (write-pointer p w))

(defmethod print-method CharPointer
  [p w]
  (write-pointer p w))

(defmethod print-method KeywordPointer
  [p w]
  (write-pointer p w))

;; ================================ Fluokitten ==================================

(defn ^:private check-sizes
  ([_]
   true)
  ([x y]
   (<= (size x) (size y)))
  ([x y z]
   (<= (size x) (min (size y) (size z))))
  ([x y z v]
   (<= (size x) (min (size y) (size z) (size v))))
  ([x y z v w]
   (<= (size x) (min (size y) (size z) (size v) (size w)))))

(def ^{:no-doc true :const true :private true} SIZES_MSG
  "Pointer should have compatible sizes.")

(defmacro ^:private map-entries-i [ptype f i & xs]
  `(~f ~@(map #(list `get* ptype % i) xs)))

(defmacro ^:private pointer-fmap* [ptype res f & xs]
  (if (< (count xs) 5)
    `(do
       (if (check-sizes ~res ~@xs)
         (dotimes [i# (size ~res)]
           (put* ~ptype ~res i# (map-entries-i ~ptype ~f i# ~@xs)))
         (dragan-says-ex SIZES_MSG {:xs (map info ~xs)}))
       ~res)
    `(dragan-says-ex "Pointer fmap supports up to 4 pointers.")))

(defmacro ^:private pointer-reduce* [ptype etype acctype f accfn init & xs]
  (if (< (count xs) 5)
    `(if (check-sizes ~@xs)
       (let [size-x# (size ~(first xs))]
         (loop [i# 0 acc# (~acctype ~init)]
           (if (< i# size-x#)
             (recur (inc i#) (~acctype (~f acc# (~etype (map-entries-i ~ptype ~accfn i# ~@xs)))))
             acc#)))
       (dragan-says-ex SIZES_MSG {:xs (map info ~xs)}))
    `(dragan-says-ex "Pointer reduction supports up to 4 pointers.")))

(defmacro ^:private pointer-map-reduce* [ptype etype acctype f init g & xs]
  (if (< (count xs) 5)
    `(if (check-sizes ~@xs)
       (let [size-x# (size ~(first xs))]
         (loop [i# 0 acc# (~acctype ~init)]
           (if (< i# size-x#)
             (recur (inc i#) (~acctype (~f acc# (~etype (map-entries-i ~ptype ~g i# ~@xs)))))
             acc#)))
       (dragan-says-ex SIZES_MSG {:xs (map info ~xs)}))
    `(dragan-says-ex "Pointer foldmap supports up to 4 pointers.")))

(defmacro ^:private pointer-map-reduce-indexed* [ptype etype acctype f init g & xs]
  (if (< (count xs) 5)
    `(if (check-sizes ~@xs)
       (let [size-x# (size ~(first xs))]
         (loop [i# 0 acc# (~acctype ~init)]
           (if (< i# size-x#)
             (recur (inc i#) (~acctype (~f acc# i# (~etype (map-entries-i ~ptype ~g i# ~@xs)))))
             acc#)))
       (dragan-says-ex SIZES_MSG {:xs (map str ~xs)}))
    `(dragan-says-ex "Pointer foldmap supports up to 4 vectors.")))

(defmacro ^:private pointer-fmap
  ([creator ptype]
   `(fn
      ([x# f#]
       (let-release [res# (~creator x#)]
         (pointer-fmap* ~ptype res# f# x#)))
      ([x# f# y#]
       (let-release [res# (~creator x#)]
         (pointer-fmap* ~ptype res# f# x# y#)))
      ([x# f# y# z#]
       (let-release [res# (~creator x#)]
         (pointer-fmap* ~ptype res# f# x# y# z#)))
      ([x# f# y# z# v#]
       (let-release [res# (~creator x#)]
         (pointer-fmap* ~ptype res# f# x# y# z# v#)))
      ([x# f# y# z# v# es#]
       (dragan-says-ex "Pointer fmap supports up to 4 pointers."))))
  ([ptype]
   `(let [fmap-fn# (pointer-fmap raw* ~ptype)]
      (fn
        ([x# f#]
         (fmap-fn# x# f#))
        ([x# f# xs#]
         (apply fmap-fn# x# f# xs#))))))

(defmacro ^:private pointer-fold [ptype etype acctype accfn]
  `(fn
     ([x#]
      (fold x# ~accfn (~accfn)))
     ([x# f# init#]
      (if (number? init#)
        (pointer-reduce* ~ptype ~etype ~acctype f# ~accfn init# x#)
        (pointer-reduce* ~ptype ~etype identity f# ~accfn init# x#)))
     ([x# f# init# y#]
      (if (number? init#)
        (pointer-reduce* ~ptype ~etype ~acctype f# ~accfn init# x# y#)
        (pointer-reduce* ~ptype ~etype identity f# ~accfn init# x# y#)))
     ([x# f# init# y# z#]
      (if (number? init#)
        (pointer-reduce* ~ptype ~etype ~acctype f# ~accfn init# x# y# z#)
        (pointer-reduce* ~ptype ~etype identity f# ~accfn init# x# y# z#)))
     ([x# f# init# y# z# v#]
      (if (number? init#)
        (pointer-reduce* ~ptype ~etype ~acctype f# ~accfn init# x# y# z# v#)
        (pointer-reduce* ~ptype ~etype identity f# ~accfn init# x# y# z# v#)))
     ([_# _# _# _# _# _# _#]
      (dragan-says-ex "fold with more than four arguments is not available for pointers."))))

(defmacro ^:private pointer-foldmap [ptype etype acctype indexed-fn accfn]
  `(fn
     ([x# g#]
      (foldmap x# g# ~accfn (~accfn)))
     ([x# g# f# init#]
      (if (number? init#)
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~acctype f# init# g# x#)
          (pointer-map-reduce* ~ptype ~etype ~acctype f# init# g# x#))
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~identity f# init# g# x#)
          (pointer-map-reduce* ~ptype ~etype identity f# init# g# x#))))
     ([x# g# f# init# y#]
      (if (number? init#)
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~acctype f# init# g# x# y#)
          (pointer-map-reduce* ~ptype ~etype ~acctype f# init# g# x# y#))
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~identity f# init# g# x# y#)
          (pointer-map-reduce* ~ptype ~etype identity f# init# g# x# y#))))
     ([x# g# f# init# y# z#]
      (if (number? init#)
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~acctype f# init# g# x# y# z#)
          (pointer-map-reduce* ~ptype ~etype ~acctype f# init# g# x# y# z#))
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~identity f# init# g# x# y# z#)
          (pointer-map-reduce* ~ptype ~etype identity f# init# g# x# y# z#))))
     ([x# g# f# init# y# z# v#]
      (if (number? init#)
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~acctype f# init# g# x# y# z# v#)
          (pointer-map-reduce* ~ptype ~etype ~acctype f# init# g# x# y# z# v#))
        (if (instance? ~indexed-fn g#)
          (pointer-map-reduce-indexed* ~ptype ~etype ~identity f# init# g# x# y# z# v#)
          (pointer-map-reduce* ~ptype ~etype identity f# init# g# x# y# z# v#))))
     ([_# _# _# _# _# _# _# _#]
      (dragan-says-ex "foldmap with more than four arguments is not available for pointers."))))

(defn ^:private pointer-op [create-pointer]
  (fn [x & ws]
    (let-release [res (create-pointer (transduce (map size) + (size x) ws))]
      (loop [pos 0 w x ws ws]
        (when w
          (if (instance? (class res) w)
            (memcpy! w (get-pointer res pos) (bytesize w))
            (dragan-says-ex "You can not apply op on pointers of different type." {:res res :w w}))
          (recur (+ pos (size w)) (first ws) (next ws))))
      res)))

(defmacro ^:private extend-pointer-fluokitten [t constructor cast indexed-fn accfn]
  `(extend ~t
     Functor
     {:fmap (pointer-fmap ~t)}
     PseudoFunctor
     {:fmap! (pointer-fmap identity ~t)}
     Foldable
     {:fold (pointer-fold ~t ~cast ~cast ~accfn)
      :foldmap (pointer-foldmap ~t ~cast ~cast ~indexed-fn ~accfn)}
     Magma
     {:op (constantly (pointer-op ~constructor))}))

(defn ^:private or-fn [& xs]
  (some identity xs))

(defn ^:private error [& _]
  (dragan-says-ex "This pointer type does not support arithmetic."))

(extend-pointer-fluokitten BytePointer byte-pointer byte IFn$LLL +)
(extend-pointer-fluokitten CLongPointer clong-pointer long IFn$LLL +)
(extend-pointer-fluokitten SizeTPointer size-t-pointer long IFn$LLL +)
(extend-pointer-fluokitten BoolPointer bool-pointer boolean IFn$LOO or-fn)
(extend-pointer-fluokitten PointerPointer pointer-pointer pointer IFn$LOO error)
(extend-pointer-fluokitten CharPointer char-pointer char IFn$LOO error)
(extend-pointer-fluokitten ShortPointer short-pointer short IFn$LLL +)
(extend-pointer-fluokitten IntPointer int-pointer int IFn$LLL +)
(extend-pointer-fluokitten LongPointer long-pointer long IFn$LLL +)
(extend-pointer-fluokitten FloatPointer float-pointer float IFn$LDD +)
(extend-pointer-fluokitten DoublePointer double-pointer double IFn$LDD +)
