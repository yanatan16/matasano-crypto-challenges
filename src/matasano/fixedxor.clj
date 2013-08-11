(ns matasano.fixedxor
	(:require [matasano.util :as util]))

(defn xor [b1 b2]
	(map bit-xor b1 b2))

(defn xor-at [b1 at b2]
	(->>
		b2
		(xor (nthrest b1 at))
		(concat (take at b1))))

(defn xor-mid [b1 at b2]
	(concat (xor-at b1 at b2) (nthrest b1 (+ at (count b2)))))

(defn solve
	"De-hex and encode base64 a string"
	[h1 h2 & more]
  (util/hexify
  	(xor
  		(util/unhexify h1)
  		(util/unhexify h2))))