(ns matasano.util
	(:require [clojure.data.codec.base64 :as b64])
	(:require [clojure.java.io :as io])
	(:import java.security.SecureRandom))

(defn ubyte [val]
   (if (>= val 128)
     (byte (- val 256))
     (byte val)))

(defn un-ubyte [val]
	(if (< val 0)
		(int (+ val 256))
		(int val)))

(defn byte-string
	"Convert char string to byte string"
	[chars]
	(map int chars))

(defn true-byte-string [seq]
	(byte-array
		(map (comp ubyte int) seq)))

(defn char-string
	"Convert byte string to char string"
	[& byte-arrays]
	(apply str
		(map #(apply str (map char %)) byte-arrays)))

(defn long-string [bytes]
	(map
		(fn [[a b c d]]
			(+ (bit-shift-left a 24) (bit-shift-left b 16) (bit-shift-left c 8) d))
		(partition 4 bytes)))

(defn unhexify
	"Convert a hex char string to a byte string"
	[hex]
  (map
    (fn [[x y]] (Integer/parseInt (str x y) 16))
    (partition 2 hex)))

(defn hexify
	"Convert a byte string to a hex-string"
	[s]
	(apply str
  	(map #(format "%02x" (int %))
  		(char-string s))))

(defn unbase64ify
	"Convert a base64 char string to a byte string"
	[strng]
	(byte-string
		(b64/decode
			(true-byte-string strng))))

(defn base64ify
	"Convert a byte string to a base64 char string"
	[bytes]
	(char-string
		(b64/encode
			(true-byte-string bytes))))

(defn get-lines
	"Open a file and get the lines"
	[fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn slice
	"Take m values after n values"
	[n m seq]
	(take m (nthrest seq n)))

(defn splice
	"Replace m values of seq with newseq into seq at position n. (m=(count seq) by default)"
	([n seq newseq]
		(splice n (count newseq) seq newseq))
	([n m seq newseq]
		(concat (take n seq) newseq (nthrest seq (+ n m)))))

(defn rand-bytes
  "Returns a random byte array of the specified size."
  [size]
  (let [seed (byte-array size)]
    (.nextBytes (SecureRandom/getInstance "SHA1PRNG") seed)
    seed))

(defn map= [& args] (every? identity (apply map = args)))

(defn rotate [seq n]
	(let [m (mod n (count seq))]
		(concat (nthrest seq m) (take m seq))))

(defn- int-shift-byte [x n]
	(bit-and 0xff (bit-shift-right x n)))

(defn int-bigend-64 [x]
	(map (partial int-shift-byte x) (range 56 -1 -8)))

(defn int-bigend-32 [x]
	(map (partial int-shift-byte x) (range 24 -1 -8)))

(defn int-lilend-64 [x]
	(map (partial int-shift-byte x) (range 0 64 8)))

(defn unix [] (quot (System/currentTimeMillis) 1000))

(defn printret
	([arg] (println arg) arg)
	([arg & args]	(apply println (cons arg args)) (cons arg args)))