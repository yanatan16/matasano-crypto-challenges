(ns matasano.router
	(:require [matasano.base64 :as base64])
	(:require [matasano.fixedxor :as fixedxor])
	(:require [matasano.xorcipher :as xorcipher])
	(:require [matasano.find-xor-cipher :as findxor])
	(:require [matasano.rep-xor :as repxor])
	(:require [matasano.aes :as aes])
	(:require [matasano.english :as english])
	(:require [matasano.attack-aes :as attack-aes])
	(:require [matasano.aes-ctr :as aes-ctr])
	(:gen-class :main true))

(def problems {
	1 base64/solve
	2 fixedxor/solve
	3 xorcipher/solve
	4 findxor/solve
	5 repxor/solve-encrypt
	6 repxor/solve-decrypt
	7 aes/solve-decrypt
	8 english/solve-find-ciphered
	10 aes/solve-cbc-decrypt
	11 attack-aes/guess-cbc
	12 attack-aes/solve-break-oracle-ecb
	14 attack-aes/solve-break-oracle-ecb-2
	18 aes-ctr/solve-crypt
	19 aes-ctr/solve-crypt-multi
	20 aes-ctr/solve-crypt-multi
	25 aes-ctr/solve-exploit-edit
})

(defn -main
	"Mux between multiple problems"
	[k & args]
	(->
		k
		Integer/parseInt
		problems
		(apply args)))
