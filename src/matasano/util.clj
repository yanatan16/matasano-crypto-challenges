(ns matasano.util
	(:require [clojure.data.codec.base64 :as b64])
	(:require [clojure.java.io :as io]))

(defn ubyte [val]
   (if (>= val 128)
     (byte (- val 256))
     (byte val)))

(defn byte-string
	"Convert char string to byte string"
	[chars]
	(map int chars))

(defn char-string
	"Convert byte string to char string"
	[bytes]
	(apply str
		(map char bytes)))

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
		(b64/decode strng)))

(defn base64ify
	"Convert a byte string to a base64 char string"
	[bytes]
	(char-string
		(b64/encode
			(byte-array
				(map ubyte bytes)))))

(defn get-lines
	"Open a file and get the lines"
	[fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

