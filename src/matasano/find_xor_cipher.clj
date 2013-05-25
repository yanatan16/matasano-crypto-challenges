(ns matasano.find-xor-cipher
	(:require [matasano.util :as util])
	(:require [matasano.xorcipher :as xor])
	(:require [matasano.english :as english]))

(defn find-message [lines]
	(let [plains (map #(list (xor/decipher (util/unhexify %)) %) lines)
				fplains (filter (comp (complement nil?) first) plains)
				scores (map (fn [[a b]] [(english/score a) (util/char-string a) b]) fplains)]
		(first (sort-by first scores))))

(defn solve
	"Find an xor-cipher from a file of encoded texts"
	[file & more]
	(second
		(find-message
			(util/get-lines file))))