(ns matasano.aes-ctr
	(:use clojure.contrib.core)
	(:require [clojure.string :as str])
	(:require [matasano.util :as util])
  (:require [matasano.aes :as aes])
  (:require [matasano.fixedxor :as xor])
  (:require [matasano.rep-xor :as rep-xor]))

(defn make-instance [key nonce plain]
	{:key key, :nonce nonce, :count 0, :plain plain})

(defn encrypt-block [instance]
	(let [block (mapcat util/int-lilend [(instance :nonce) (instance :count)])
				eblock (aes/raw-encrypt (instance :key) block)
				pblock (take 16 (instance :plain))]
		[(xor/xor pblock eblock)
		 (assoc instance :count (inc (instance :count))
										 :plain (nthrest (instance :plain) 16))]))

(defn encrypt-all
	([instance]
		(encrypt-all instance []))
	([instance cipher]
		(if
			(empty? (instance :plain))
			[cipher instance]
			(let [[nblock ninstance] (encrypt-block instance)]
				(encrypt-all ninstance (concat cipher nblock))))))

(defn encrypt [key nonce plain]
	(->>
		plain
		(make-instance key nonce)
		encrypt-all
		first))

(def decrypt-block encrypt-block)
(def decrypt-all encrypt-all)
(def decrypt encrypt)

(defn solve-crypt [key nonce text]
	(util/char-string
			(encrypt
				(util/byte-string key)
				(Integer/parseInt nonce)
				(util/unbase64ify text))))

(defn solve-crypt-multi [file]
	(let [key (aes/rand-key)
				ciphers (map util/unbase64ify (util/get-lines file))
				len (apply min (map count ciphers))
				xor-decipher #(second (rep-xor/decipher-and-score % len))]
		(-?>>
			ciphers
			(map #(take len %))
			(apply concat)
			xor-decipher
			util/char-string
			(partition len)
			(map #(apply str %)))))