(ns matasano.router
	(:require [matasano.base64 :as base64])
	(:require [matasano.fixedxor :as fixedxor])
	(:require [matasano.xorcipher :as xorcipher])
	(:require [matasano.find-xor-cipher :as findxor])
	(:gen-class :main true))

(defn -main
	"Mux between multiple problems"
	[k & args]
	(let
		[problems {
				:one base64/solve,
				:two fixedxor/solve,
				:three xorcipher/solve,
				:four findxor/solve
			}]
		(println
			(apply (problems (keyword k)) args))))