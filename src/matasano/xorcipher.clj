(ns matasano.xorcipher
	(:require [matasano.util :as util])
	(:require [matasano.english :as english]))

(defn find-keys [bcipher]
	(let [counts (map first (reverse (sort-by second (frequencies bcipher))))]
		(set (flatten
			(map
				(fn [c] (map #(bit-xor (int %) c) [\a \e \i \o \u \A \E \I \O \U (first " ")]))
				counts)))))

(defn all-keys []
	(range 256))

(defn decode [k bcipher]
	(util/byte-string
		(map #(bit-xor k %) bcipher)))

(def encode decode)

(defn find-key [bcipher]
	(let [keylist (find-keys bcipher)
				fkeylist (filter #(english/valid-english-chars? (decode % bcipher)) keylist)
				scores (sort-by first (map #(list (english/score (decode % bcipher)) %) fkeylist))]
		(second (first scores))))

(defn decipher [bcipher]
	(let [k (find-key bcipher)]
		(if (not (nil? k))
			(decode k bcipher)
			nil)))

(defn solve
	"Decode a single-byte xor-encoded cipher"
	[cipher & more]
	(let [bcipher (util/unhexify cipher)]
		(util/char-string
			(decipher bcipher))))