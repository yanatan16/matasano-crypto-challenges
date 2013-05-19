(ns matasano.fixedxor
	(:require [matasano.util :as util]))

(defn xor [b1 b2]
	(map bit-xor b1 b2))

(defn solve
	"De-hex and encode base64 a string"
	[h1 h2 & more]
  (util/hexify
  	(xor
  		(util/unhexify h1)
  		(util/unhexify h2))))