;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-cpp
  (:require [uncomplicate.commons
             [core :refer [Releaseable release let-release Info info]]
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

(defn max-bytes ^long []
  (Pointer/maxBytes))

;; ================= Memory =================================

(defn malloc! [^long size]
  (Pointer/malloc size))

(defn calloc! [^long n ^long size]
  (Pointer/malloc size))

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
  (.fill p b))

;; ================= Pointer =================================

(defn address [^Pointer p]
  (.address p))

(defn null? [^Pointer p]
  (Pointer/isNull p))

(defn capacity ^long [^Pointer p]
  (.capacity p))

#_(defn capacity! [^Pointer p ^long capacity]
    (.capacity p capacity))

(defn limit ^long [^Pointer p]
  (.limit p))

#_(defn limit! [^Pointer p ^long limit]
    (.limit p limit))

(defn position ^long [^Pointer p]
  (.position p))

#_(defn position! [^Pointer p ^long position]
    (.position p position))

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
      true)))

(defprotocol PointerCreator
  (pointer [this]))

(defprotocol TypedPointerCreator
  (pointer-pointer [this])
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
  (put-pointer* [src dst]))

(extend-type nil
  PointerCreator
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

(extend-protocol PointerCreator
  Pointer
  (pointer [this]
    (Pointer. this)))

(defmacro create-new* [constructor arg-type arg]
  `(new ~constructor ~(with-meta arg {:tag arg-type})))

(defmacro extend-creator
  ([ct]
   `(extend-creator ~ct ~ct))
  ([ct arg-type]
   `(extend-type ~ct
      TypedPointerCreator
      (byte-pointer [this#]
        (create-new* BytePointer ~arg-type this#))
      (clong-pointer [this#]
        (create-new* CLongPointer ~arg-type this#))
      (size-t-pointer [this#]
        (create-new* SizeTPointer ~arg-type this#))
      (pointer-pointer [this#]
        (create-new* PointerPointer ~arg-type this#))
      (char-pointer [this#]
        (create-new* CharPointer ~arg-type this#))
      (short-pointer [this#]
        (create-new* ShortPointer ~arg-type this#))
      (int-pointer [this#]
        (create-new* IntPointer ~arg-type this#))
      (long-pointer [this#]
        (create-new* LongPointer ~arg-type this#))
      (float-pointer [this#]
        (create-new* FloatPointer ~arg-type this#))
      (double-pointer [this#]
        (create-new* DoublePointer ~arg-type this#)))))

(extend-creator Pointer)
(extend-creator Character long)
(extend-creator Byte long)
(extend-creator Short long)
(extend-creator Integer long)
(extend-creator Long)

(extend-type Buffer
  PointerCreator
  (pointer [b]
    (Pointer. b)))

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
    (CharPointer. (char-array this)))
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

(defmacro extend-typed-creator
  ([creator-class pt method]
   `(extend-typed-creator ~creator-class ~creator-class ~pt ~method))
  ([creator-class creator-type pt method]
   `(extend-type ~creator-class
       PointerCreator
       (pointer [this#]
         (create-new* ~pt ~creator-type this#))
       TypedPointerCreator
       (~method [this#]
        (create-new* ~pt ~creator-type this#)))))

(extend-typed-creator CharBuffer CharPointer char-pointer)
(extend-typed-creator ByteBuffer BytePointer byte-pointer)
(extend-typed-creator ShortBuffer ShortPointer short-pointer)
(extend-typed-creator IntBuffer IntPointer int-pointer)
(extend-typed-creator LongBuffer LongPointer long-pointer)
(extend-typed-creator FloatBuffer FloatPointer float-pointer)
(extend-typed-creator DoubleBuffer DoublePointer double-pointer)
(extend-typed-creator (Class/forName "[Ljava.lang.Character;") chars CharPointer char-pointer)
(extend-typed-creator (Class/forName "[B") bytes BytePointer byte-pointer)
(extend-typed-creator (Class/forName "[S") shorts ShortPointer short-pointer)
(extend-typed-creator (Class/forName "[I") ints IntPointer int-pointer)
(extend-typed-creator (Class/forName "[F") floats FloatPointer float-pointer)
(extend-typed-creator (Class/forName "[D") doubles DoublePointer double-pointer)

(defmacro extend-pointer-pointer [array-class]
  `(extend-type ~array-class
     TypedPointerCreator
     (pointer-pointer [this#]
       (create-new* PointerPointer ~array-class this#))))

(extend-pointer-pointer (Class/forName "[[B"))
(extend-pointer-pointer (Class/forName "[Ljava.lang.Character;"))
(extend-pointer-pointer (Class/forName "[[S"))
(extend-pointer-pointer (Class/forName "[[I"))
(extend-pointer-pointer (Class/forName "[[J"))
(extend-pointer-pointer (Class/forName "[[F"))
(extend-pointer-pointer (Class/forName "[[D"))

(extend-type (Class/forName "[J")
  PointerCreator
  (pointer [this]
    (LongPointer. ^longs this))
  TypedPointerCreator
  (clong-pointer [this]
    (CLongPointer. ^longs this))
  (size-t-pointer [this]
    (SizeTPointer. ^longs this))
  (long-pointer [this]
    (LongPointer. ^longs this)))

(extend-type String
  PointerCreator
  (byte-pointer
    ([s]
     (BytePointer. s))
    ([s charset]
     (if (string? charset)
       (BytePointer. s ^String charset)
       (BytePointer. s ^Charset charset)))))

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

(defmacro access*
  ([method pt dst src]
   `(. ~(with-meta dst {:tag pt}) ~method ~(with-meta src {:tag Pointer})))
  ([method pt p val-type a]
   `(. ~(with-meta p {:tag pt}) ~method ~(with-meta a {:tag val-type})))
  ([method pt p val-type a offset length]
   `(. ~(with-meta p {:tag pt}) ~method ~(with-meta a {:tag val-type}) ~offset ~length)))

(extend-type Pointer
  PointerCreator
  (pointer [this]
    (Pointer. this))
  TypedPointerCreator
  (pointer-pointer [this]
    (PointerPointer. this))
  PutPointer
  (put-pointer*
    ([src dst]
     (.put ^Pointer dst src))))

(defmacro extend-pointer [pt entry-type array-type]
  `(extend-type ~pt
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
        (.put p# (~array-type obj#)))
       ([this# i# x#]
        (.put this# (long i#) x#))
       ([p# arr# offset# length#]
        (access* put ~pt p# ~array-type arr# offset# length#)))))

(extend-pointer CLongPointer long longs)
(extend-pointer SizeTPointer long longs)
(extend-pointer BytePointer byte bytes)
(extend-pointer CharPointer char chars)
(extend-pointer ShortPointer short shorts)
(extend-pointer IntPointer int ints)
(extend-pointer LongPointer long longs)
(extend-pointer FloatPointer float floats)
(extend-pointer DoublePointer double doubles)
