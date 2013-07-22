(ns matasano.aes-ctr
	(:use clojure.contrib.core)
	(:require [clojure.string :as str])
	(:require [matasano.util :as util])
  (:require [matasano.aes :as aes])
  (:require [matasano.fixedxor :as xor])
  (:require [matasano.rep-xor :as rep-xor]))

(defn make-instance [key nonce]
	{:key key, :nonce nonce, :count 0})

(defn- get-block [instance]
		[
			(->>
				(map instance [:nonce :count])
				(mapcat util/int-lilend)
				(aes/raw-encrypt (instance :key)))
			(assoc instance :count (inc (instance :count)))])

(defn stream [instance]
	(let [[b i] (get-block instance)]
		(concat b (lazy-seq (stream i)))))

(defn encrypt-all [instance plain]
	(xor/xor (stream instance) plain))

(defn encrypt [key nonce plain]
	(encrypt-all (make-instance key nonce) plain))

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