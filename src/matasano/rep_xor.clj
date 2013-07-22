(ns matasano.rep-xor
	(:require [matasano.util :as util])
	(:require [clojure.string :as string])
	(:require [matasano.cryptotools :as crypto])
	(:require [matasano.xorcipher :as xor])
	(:require [matasano.english :as english]))

(defn encode [key text]
	(let [bkey (util/byte-string key)
				btxt (util/byte-string text)]
		(util/char-string
			(map bit-xor (cycle bkey) btxt))))
(def decode encode)

(defn encode-multi [key & texts]
	(map (partial encode key) texts))

(defn- guess-keylens [text]
	(let [compares
					(map
						#(list
							%
							(take % text)
							(util/slice % % text)
							(util/slice (* 2 %) % text)
							(util/slice (* 3 %) % text))
						(range 2 41))
				distances (map (fn [[n & args]] [(apply crypto/mean-pairwise crypto/hamming-distance args) n]) compares)
				norms (map (fn [[d n]] [(/ d n) n]) distances)]
		(take 2 (map second (sort-by first norms)))))

(defn decipher-and-score [text len]
	(let [partitions (apply map vector (partition len text))
				key (map #(xor/find-key %) partitions)
				plaintext (if (every? (complement nil?) key) (decode key text) nil)]
		(if (nil? plaintext)
			nil
			(list (english/score plaintext) (util/char-string plaintext)))))

(defn solve-decrypt
	"Decode a large string encoded with repeating-xor"
	[file & more]
	(let [text (util/unbase64ify (apply str (util/get-lines file)))
				keylens (guess-keylens text)
				scores (map (partial decipher-and-score text) keylens)
				best (first (sort-by first (filter (complement nil?) scores)))]
		(util/char-string (second best))))

(defn solve-encrypt
	"Encode a string with repeating-xor"
	[key & files]
	(let [inputs (map #(string/join "\n" (util/get-lines %)) files)]
		(string/join "\n"
			(map util/hexify
				(apply encode-multi key inputs)))))