(ns matasano.english
	(:require [matasano.util :as util])
	(:require [clojure.string :as string])
	(:require [clojure.contrib.math :as math]))

(def plaintext-limit (float 0.75))
(def ciphertext-limit (float 0.7))

(defn valid-english-chars?	[s]
	(every? #(or (= 9 %) (= 10 %) (and (< 31 %) (> 127 %))) s))

(def english-frequencies
	{
		\a	0.08167
		\b	0.01492
		\c	0.02782
		\d	0.04253
		\e	0.102702
		\f	0.02228
		\g	0.02015
		\h	0.06094
		\i	0.06966
		\j	0.00153
		\k	0.00772
		\l	0.04025
		\m	0.02406
		\n	0.06749
		\o	0.07507
		\p	0.01929
		\q	0.00095
		\r	0.05987
		\s	0.06327
		\t	0.09056
		\u	0.02758
		\v	0.00978
		\w	0.02360
		\x	0.00150
		\y	0.01974
		\z	0.00074
	})

(def english-digraph-frequencies
	{
		"th"	0.0152
		"he"	0.0128
		"in"	0.0094
		"er"	0.0094
		"an"	0.0082
		"re"	0.0068
		"nd"	0.0063
		"at"	0.0059
		"on"	0.0057
		"nt"	0.0056
		"ha"	0.0056
		"es"	0.0056
		"st"	0.0055
		"en"	0.0055
		"ed"	0.0053
		"to"	0.0052
		"it"	0.0050
		"ou"	0.0050
		"ea"	0.0047
		"hi"	0.0046
		"is"	0.0046
		"or"	0.0043
		"ti"	0.0034
		"as"	0.0033
		"te"	0.0027
		"et"	0.0019
		"ng"	0.0018
		"of"	0.0016
		"al"	0.0009
		"de"	0.0009
		"se"	0.0008
		"le"	0.0008
		"sa"	0.0006
		"si"	0.0005
		"ar"	0.0004
		"ve"	0.0004
		"ra"	0.0004
		"ld"	0.0002
		"ur"	0.0002
	})

(defn- plaintext-letter-score [s]
	(let [ss (string/lower-case (string/replace (util/char-string s) #"[^a-zA-z]" ""))
				size (count ss)]
		(/
			(apply +
				(map
					(fn [[key val]]
						(math/abs
							(-
								(/ val size)
								(let [ev (english-frequencies key)]
									(if ev ev 0)))))
					(frequencies ss)))
			size)))

(defn- plaintext-digraph-score [s]
	(let [ss (string/lower-case (util/char-string s))
				size (- (count ss) 1)
				digraphs (map #(subs ss % (+ % 2)) (range size))]
		(/
			(apply +
				(map
					(fn [[key val]]
						(math/abs
							(-
								(/ val size)
								(let [ev (english-digraph-frequencies key)]
									(if ev ev 0)))))
					(frequencies digraphs)))
			size)))

(defn- ciphertext-letter-score [s]
	(let [size (.size s)
				ofreqs (reverse (sort (map #(/ % size) (vals (frequencies s)))))
				cfreqs (reverse (sort (vals english-frequencies)))]
		(/
			(apply +
				(map (comp math/abs -) ofreqs cfreqs))
			size)))

(defn score [s]
	(plaintext-letter-score s))

(defn ciphertext-score [s]
	(ciphertext-letter-score s))

(defn solve-find-ciphered
  [file & more]
  (let [lines (map util/unhexify (util/get-lines file))
        scores (sort-by first (map #(list (ciphertext-score %) %) lines))
        best  (first scores)]
    (util/hexify (second best))))