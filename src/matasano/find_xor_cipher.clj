(ns matasano.find-xor-cipher
	(:require [matasano.util :as util])
	(:require [matasano.xorcipher :as xor])
	(:require [matasano.english :as english]))

(defn find-message [lines]
	(let [bests (filter #(not (nil? (first %))) (map #(list (xor/decipher (util/unhexify %)) %) lines))
				scores (map #(list (english/plaintext-score (first %)) (util/char-string (first %)) (second %)) bests)]
		(first (sort-by first scores))))

(defn solve
	"Find an xor-cipher from a file of encoded texts"
	[file & more]
	(rest
		(find-message
			(util/get-lines file))))