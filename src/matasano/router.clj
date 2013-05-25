(ns matasano.router
	(:require [matasano.base64 :as base64])
	(:require [matasano.fixedxor :as fixedxor])
	(:require [matasano.xorcipher :as xorcipher])
	(:require [matasano.find-xor-cipher :as findxor])
	(:require [matasano.rep-xor :as repxor])
	(:require [matasano.aes :as aes])
	(:require [matasano.english :as english])
	(:gen-class :main true))

(defn -main
	"Mux between multiple problems"
	[k & args]
	(let
		[problems {
				1 base64/solve
				2 fixedxor/solve
				3 xorcipher/solve
				4 findxor/solve
				5 repxor/solve-encrypt
				6 repxor/solve-decrypt
				7 aes/solve-decrypt
				8 english/solve-find-ciphered
			}]
		(println
			(apply (problems (Integer/parseInt k)) args))))
