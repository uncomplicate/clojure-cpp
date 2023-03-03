;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-cpp
  (:require [uncomplicate.commons
             [core :refer [Releaseable release let-release Info info Viewable view]]
             [utils :refer [dragan-says-ex]]])
  (:import [java.nio Buffer ByteBuffer CharBuffer ShortBuffer IntBuffer LongBuffer FloatBuffer
            DoubleBuffer]
           java.nio.charset.Charset
           clojure.lang.Seqable
           [org.bytedeco.javacpp Pointer BytePointer CharPointer BoolPointer ShortPointer
            IntPointer LongPointer FloatPointer DoublePointer CLongPointer FunctionPointer
            PointerPointer SizeTPointer PointerScope]))

;; ================= System =================================

(defn physical-bytes
  (^long []
   (Pointer/physicalBytes))
  (^long [^long max-size]
   (Pointer/physicalBytesInaccurate max-size)))

(defn available-physical-bytes ^long []
  (Pointer/availablePhysicalBytes))

(defn max-physical-bytes ^long []
  (Pointer/maxPhysicalBytes))

(defn total-physical-bytes ^long []
  (Pointer/totalPhysicalBytes))

(defn tracked-bytes ^long []
  (Pointer/totalBytes))

(defn max-tracked-bytes ^long []
  (Pointer/maxBytes))

(defn pointers-count ^long []
  (Pointer/totalCount))

;; ================= Memory =================================

(defn malloc! [^long size]
  (Pointer/malloc size))

(defn calloc! [^long n ^long element-size]
  (Pointer/calloc n element-size))

(defn realloc! [^Pointer p ^long size]
  (Pointer/realloc p size))

(defn free! [^Pointer p]
  (Pointer/free p)
  (.setNull p)
  p)

(defn memcmp ^long [^Pointer p1 ^Pointer p2 ^long size]
  (Pointer/memcmp p1 p2 size))

(defn memcpy! [^Pointer dst ^Pointer src ^long size]
  (Pointer/memcpy dst src size))

(defn memmove! [^Pointer dst ^Pointer src ^long size]
  (Pointer/memmove dst src size))

(defn memset! [^Pointer dst ch ^long size]
  (Pointer/memset dst (int ch) size))

(defn fill! [^Pointer p b]
  (.fill p b)
  p)

(defn zero! [^Pointer p]
  (.zero p)
  p)

;; ================= Pointer =================================

(defn address [^Pointer p]
  (.address p))

(defn null? [^Pointer p]
  (Pointer/isNull p))

(defn capacity ^long [^Pointer p]
  (.capacity p))

(defn capacity! [^Pointer p ^long n]
  (.capacity p n))

(defn limit ^long [^Pointer p]
  (.limit p))

(defn limit! [^Pointer p ^long n]
  (.limit p n))

(defn position ^long [^Pointer p]
  (.position p))

(defn position! [^Pointer p ^long n]
  (.position p n))

(defn element-count ^long [^Pointer p]
  (max 0 (- (.limit p) (.position p))))

(defn sizeof ^long [^Pointer p]
  (.sizeof p))

(defn get-pointer
  ([^Pointer p]
   (.getPointer p))
  ([^Pointer p ^long i]
   (.getPointer p i))
  ([^Pointer p type ^long i]
   (.getPointer p type i)))

;; ================= Buffer =================================

(defn byte-buffer [^Pointer p]
  (.asByteBuffer p))

(defn buffer [^Pointer p]
  (.asBuffer p))

(defprotocol PointerCreator
  (pointer [this] [this i]))

(defprotocol TypedPointerCreator
  (pointer-pointer [this] [this charset])
  (byte-pointer [this] [this charset])
  (bool-pointer [this])
  (clong-pointer [this])
  (size-t-pointer [this])
  (char-pointer [this])
  (short-pointer [this])
  (int-pointer [this])
  (long-pointer [this])
  (float-pointer [this])
  (double-pointer [this]))

(defprotocol Accessor
  (get! [pointer dst!] [pointer dst! offset length])
  (put! [pointer! src] [pointer! i x] [pointer! src offset length])
  (get-entry [pointer] [pointer i]))

(defprotocol PutPointer
  (put-pointer* [src dst] [arg1 src dst]))

(def ^:const pointer-types
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

(def ^:const type-pointers
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
   :pointer PointerPointer
   :default Pointer
   Double/TYPE DoublePointer
   Float/TYPE FloatPointer
   Long/TYPE LongPointer
   Integer/TYPE IntPointer
   Short/TYPE ShortPointer
   Byte/TYPE BytePointer
   Character/TYPE CharPointer})

(let [get-deallocator (doto (.getDeclaredMethod Pointer "deallocator" (make-array Class 0))
                        (.setAccessible true))
      empty-array (into-array [])]
  (extend-type Pointer
    Releaseable
    (release [this]
      (if (.invoke get-deallocator this empty-array)
        (do (.deallocate this)
            (.setNull this))
        (free! this))
      true)
    Info
    (info
      ([this]
       {:address (.address this)
        :type (let [t (type this)] (get pointer-types t t))
        :position (.position this)
        :limit (.limit this)
        :capacity (.capacity this)
        :deallocator (.invoke get-deallocator this empty-array)})
      ([this info-type]
       (case info-type
         :address (.address this)
         :type (let [t (type this)] (get pointer-types t t))
         :position (.position this)
         :limit (.limit this)
         :capacity (.capacity this)
         :deallocator (.invoke get-deallocator this empty-array)
         nil)))
    PointerCreator
    (pointer
      ([this]
       (Pointer. this))
      ([this i]
       (.position (Pointer. this) i)))
    TypedPointerCreator
    (byte-pointer [this]
      (BytePointer. this))
    (clong-pointer [this]
      (CLongPointer. this))
    (size-t-pointer [this]
      (SizeTPointer. this))
    (pointer-pointer [this]
      (PointerPointer. this))
    (char-pointer [this]
      (CharPointer. this))
    (short-pointer [this]
      (ShortPointer. this))
    (int-pointer [this]
      (IntPointer. this))
    (long-pointer [this]
      (LongPointer. this))
    (float-pointer [this]
      (FloatPointer. this))
    (double-pointer [this]
      (DoublePointer. this))))

(extend-type nil
  PointerCreator
  (pointer
    ([_]
     (Pointer.))
    ([this i]
     (.position (Pointer.) i)))
  (pointer [_]
    (Pointer.))
  TypedPointerCreator
  (byte-pointer [_]
    (BytePointer.))
  (clong-pointer [_]
    (CLongPointer.))
  (size-t-pointer [_]
    (SizeTPointer.))
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

(defmacro create-new*
  ([constructor size]
   `(if (<= 0 (long ~size))
      (new ~constructor (long ~size))
      (dragan-says-ex "Array size must be 0 or larger."
                      {:size ~size})))
  ([constructor arg-type arg]
   `(new ~constructor ~(with-meta arg {:tag arg-type}))))

(defmacro extend-creator [ct]
  `(extend-type ~ct
     TypedPointerCreator
     (byte-pointer [this#]
       (create-new* BytePointer this#))
     (clong-pointer [this#]
       (create-new* CLongPointer this#))
     (size-t-pointer [this#]
       (create-new* SizeTPointer this#))
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

(extend-type Seqable
  TypedPointerCreator
  (byte-pointer [this]
    (BytePointer. (byte-array this)))
  (clong-pointer [this]
    (CLongPointer. (long-array this)))
  (size-t-pointer [this]
    (SizeTPointer. (long-array this)))
  (pointer-pointer [this]
    (pointer-pointer (into-array (class (first this)) this)))
  (char-pointer [this]
    (CharPointer. (char-array (map char this))))
  (short-pointer [this]
    (ShortPointer. (short-array this)))
  (int-pointer [this]
    (IntPointer. (int-array this)))
  (long-pointer [this]
    (LongPointer. (long-array this)))
  (float-pointer [this]
    (FloatPointer. (float-array this)))
  (double-pointer [this]
    (DoublePointer. (double-array this))))

(extend-type Buffer
  PointerCreator
  (pointer
    ([b]
     (Pointer. b))
    ([b i]
     (.position (Pointer. b) i))))

(extend-type ByteBuffer
  PointerCreator
  (pointer
    ([this]
     (BytePointer. this))
    ([this i]
     (.position (BytePointer. this) i)))
  TypedPointerCreator
  (byte-pointer [this]
    (BytePointer. this))
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

(defmacro extend-buffer [buffer-class pt method]
  `(extend-type ~buffer-class
     PointerCreator
     (pointer
       ([this#]
        (create-new* ~pt ~buffer-class this#))
       ([this# i#]
        (position! (create-new* ~pt ~buffer-class this#) i#)))
     TypedPointerCreator
     (~method [this#]
      (create-new* ~pt ~buffer-class this#))))

(extend-buffer CharBuffer CharPointer char-pointer)
(extend-buffer ByteBuffer BytePointer byte-pointer)
(extend-buffer ShortBuffer ShortPointer short-pointer)
(extend-buffer IntBuffer IntPointer int-pointer)
(extend-buffer LongBuffer LongPointer long-pointer)
(extend-buffer FloatBuffer FloatPointer float-pointer)
(extend-buffer DoubleBuffer DoublePointer double-pointer)

(defmacro access*
  ([method pt dst src]
   `(. ~(with-meta dst {:tag pt}) ~method ~(with-meta src {:tag Pointer})))
  ([method pt p val-type a]
   `(. ~(with-meta p {:tag pt}) ~method ~(with-meta a {:tag val-type})))
  ([method pt p val-type a offset length]
   `(. ~(with-meta p {:tag pt}) ~method ~(with-meta a {:tag val-type}) ~offset ~length)))

(defmacro extend-array [array-class array-type pt method]
  `(extend-type ~array-class
     PointerCreator
     (pointer
       ([this#]
        (create-new* ~pt ~array-type this#))
       ([this# i#]
        (position! (create-new* ~pt ~array-type this#) i#)))
     TypedPointerCreator
     (~method [this#]
      (create-new* ~pt ~array-type this#))
     PutPointer
     (put-pointer* [src# dst#]
       (access* put ~pt dst# ~array-type src#))))

(extend-array (Class/forName "[C") chars CharPointer char-pointer)
(extend-array (Class/forName "[B") bytes BytePointer byte-pointer)
(extend-array (Class/forName "[S") shorts ShortPointer short-pointer)
(extend-array (Class/forName "[I") ints IntPointer int-pointer)
(extend-array (Class/forName "[F") floats FloatPointer float-pointer)
(extend-array (Class/forName "[D") doubles DoublePointer double-pointer)

(extend-protocol PutPointer
  Pointer
  (put! [src dst]
    (.put ^Pointer dst ^Pointer src))
  Double
  (put-pointer* [x p]
    (.put ^DoublePointer p (double x) ))
  Float
  (put-pointer* [x p]
    (.put ^FloatPointer p (float x)))
  Long
  (put-pointer* [x p]
    (.put ^LongPointer p (long x)))
  Integer
  (put-pointer* [x p]
    (.put ^IntPointer p (int x)))
  Short
  (put-pointer* [x p]
    (.put ^ShortPointer p (short x)))
  Character
  (put-pointer* [x p]
    (.put ^CharPointer p (char x)))
  Byte
  (put-pointer* [x p]
    (.put ^BytePointer p (byte x))))

(extend-type (Class/forName "[[B")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[B" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[B" src)))

(extend-type (Class/forName "[[S")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[S" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[S" src)))

(extend-type (Class/forName "[[I")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[I" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[I" src)))

(extend-type (Class/forName "[[F")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[F" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[F" src)))

(extend-type (Class/forName "[[D")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[D" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[D" src)))

(extend-type (Class/forName "[[C")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[C" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[C" src)))

(extend-type (Class/forName "[Lorg.bytedeco.javacpp.Pointer;")
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[Lorg.bytedeco.javacpp.Pointer;" this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[Lorg.bytedeco.javacpp.Pointer;" src)))

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
  (put-pointer* [src dst]
    (.putString ^PointerPointer dst ^"[Ljava.lang.String;" src)))

(extend-type (Class/forName "[J")
  PointerCreator
  (pointer
    ([this]
     (LongPointer. ^longs this))
    ([this i]
     (.position (LongPointer. ^longs this) i)))
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. ^"[[J" this))
  (clong-pointer [this]
    (CLongPointer. ^longs this))
  (size-t-pointer [this]
    (SizeTPointer. ^longs this))
  (long-pointer [this]
    (LongPointer. ^longs this))
  PutPointer
  (put-pointer* [src dst]
    (.put ^PointerPointer dst ^"[[J" src)))

(extend-type String
  PointerCreator
  (byte-pointer
    ([s]
     (BytePointer. s))
    ([s charset]
     (if (string? charset)
       (BytePointer. s ^String charset)
       (BytePointer. s ^Charset charset))))
  PutPointer
  (put-pointer* [charset-name src dst]
    (.putString ^PointerPointer dst ^"[Ljava.lang.String;" src ^String charset-name)))

(extend-type Charset
  PutPointer
  (put-pointer* [charset src dst]
    (.putString ^PointerPointer dst ^"[Ljava.lang.String;" src ^String charset)))

(defn get-string
  ([^BytePointer p]
   (.getString p))
  ([^BytePointer p charset]
   (if (string? charset)
     (.getString p ^String charset)
     (.getString p ^Charset charset))))

(defn get-pointer-value
  ([^BytePointer p]
   (.getPointerValue p))
  ([^BytePointer p ^long i]
   (.getPointerValue p i)))

(defn set-pointer-value!
  [^BytePointer p ^long i ^Pointer x]
  (.putPointerValue p i x)
  p)

(defn get-unsigned
  (^long [^BytePointer p]
   (.getUnsigned p))
  ([^BytePointer p ^long i]
   (.getUnsigned p i)))

(defn put-unsigned! [^BytePointer p ^long i ^long x]
  (.putUnsigned p i x)
  p)

(defn get-bool
  ([^BytePointer p]
   (.getBool p))
  ([^BytePointer p ^long i]
   (.getBool p i)))

(defn put-bool! [^BytePointer p ^long i ^Boolean x]
  (.putBool p i x)
  p)

(defn get-char
  ([^BytePointer p]
   (.getChar p))
  ([^BytePointer p ^long i]
   (.getChar p i)))

(defn put-char! [^BytePointer p ^long i x]
  (.putChar p i x)
  p)

(defn get-int
  (^long [^BytePointer p]
   (.getInt p))
  (^long [^BytePointer p ^long i]
   (.getInt p (* Integer/BYTES i))))

(defn put-int! [^BytePointer p ^long i ^long x]
  (.putInt p (* Integer/BYTES i) x)
  p)

(defn get-long
  (^long [^BytePointer p]
   (.getLong p))
  (^long [^BytePointer p ^long i]
   (.getLong p (* Long/BYTES i))))

(defn put-long! [^BytePointer p ^long i ^long x]
  (.putLong p (* Long/BYTES i) x)
  p)

(defn get-byte
  (^long [^BytePointer p]
   (long (.get p)))
  (^long [^BytePointer p ^long i]
   (long (.get p (* Byte/BYTES i)))))

(defn put-byte! [^BytePointer p ^long i ^long x]
  (.put p i x)
  p)

(defn get-short
  (^long [^BytePointer p]
   (long (.getShort p)))
  (^long [^BytePointer p ^long i]
   (long (.getShort p (* Short/BYTES i)))))

(defn put-short! [^BytePointer p ^long i ^long x]
  (.putShort p (* Short/BYTES i) x)
  p)

(defn get-double
  (^double [^BytePointer p]
   (.getDouble p))
  (^double [^BytePointer p ^long i]
   (.getDouble p (* Double/BYTES i))))

(defn put-double! [^BytePointer p ^long i ^double x]
  (.putDouble p (* Double/BYTES i) x)
  p)

(defn get-float
  (^double [^BytePointer p]
   (.getFloat p))
  (^double [^BytePointer p ^long i]
   (.getFloat p (* Float/BYTES i))))

(defn put-float! [^BytePointer p ^long i ^double x]
  (.putFloat p (* Float/BYTES i) x)
  p)

(defn get-string-bytes ^bytes [^BytePointer p]
  (.getStringBytes p ))

(defn pointer-seq [^Pointer p]
  (letfn [(pointer-seq* [p ^long i ^long n]
            (lazy-seq
             (if (< -1 i n)
               (cons (get-entry p i) (pointer-seq* p (inc i) n))
               '())))]
    (if (null? p)
      nil
      (pointer-seq* p 0 (element-count p)))))

(defmacro extend-pointer [pt entry-type array-type convert-fn]
  `(extend-type ~pt
     PointerCreator
     (pointer
       ([this#]
        (create-new* ~pt ~pt this#))
       ([this# i#]
        (position! (create-new* ~pt ~pt this#) i#)))
     Accessor
     (get-entry
       ([this#]
        (.get this#))
       ([this# i#]
        (.get this# (long i#))))
     (get!
       ([p# arr#]
        (access* get ~pt p# ~array-type arr#))
       ([p# arr# offset# length#]
        (access* get ~pt p# ~array-type arr# offset# length#)))
     (put!
       ([p# obj#]
        (if (sequential? obj#)
          (access* put ~pt p# ~array-type (~convert-fn obj#))
          (put-pointer* obj# p#))
        p#)
       ([this# i# x#]
        (.put this# (long i#) (~entry-type x#)))
       ([p# arr# offset# length#]
        (access* put ~pt p# ~array-type arr# offset# length#)))))

(extend-pointer CLongPointer long longs long-array)
(extend-pointer SizeTPointer long longs long-array)
(extend-pointer BytePointer byte bytes byte-array)
(extend-pointer CharPointer char chars #(char-array (map char %)))
(extend-pointer ShortPointer short shorts short-array)
(extend-pointer IntPointer int ints int-array)
(extend-pointer LongPointer long longs long-array)
(extend-pointer FloatPointer float floats float-array)
(extend-pointer DoublePointer double doubles double-array)

(extend-type PointerPointer
  Accessor
  (get-entry
    ([this]
     (.get this))
    ([this i]
     (.get this (long i))))
  (put!
    ([p obj]
     (put-pointer* obj p)
     p)
    ([this arg1 arg2]
     (put-pointer* arg2 arg1 this)
     this))
  PutPointer
  (put-pointer* [i src dst]
    (.put ^PointerPointer dst i ^Pointer src)))
