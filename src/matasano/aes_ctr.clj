(ns matasano.aes-ctr
	(:use clojure.contrib.core)
	(:use [clojure.string :only (join)])
	(:require [clojure.string :as str])
	(:require [matasano.util :as util])
  (:require [matasano.aes :as aes])
  (:require [matasano.fixedxor :as xor])
  (:require [matasano.rep-xor :as rep-xor])
  (:require [matasano.stream :as strm])
  (:require [matasano.cookie :as cookie]))

(defn make-instance [key nonce]
	{:key key, :nonce nonce, :count 0})

(defn- get-block [instance]
		[
			(->>
				(map instance [:nonce :count])
				(mapcat util/int-lilend-64)
				(aes/raw-encrypt (instance :key)))
			(assoc instance :count (inc (instance :count)))])

(def stream (strm/block-stream get-block))
(def encrypt-all (strm/encrypt stream))

(def decrypt-all encrypt-all)


(defn encrypt
	([key plain]
		(encrypt key 0 plain))
	([key nonce plain]
		(encrypt-all (make-instance key nonce) plain)))

(def decrypt encrypt)

(defn edit [cipher key offset newplain]
	(->>
		cipher
		(decrypt key)
		(#(util/splice offset % newplain))
		(encrypt key)))
(defn editor [key]
	#(edit %1 key %2 %3))

(defn make-userdata-encoder [key pre post]
	(cookie/make-userdata-encoder (partial encrypt key) pre post))
(defn make-userdata-checker [key check-for]
	(cookie/make-userdata-checker (partial decrypt key) check-for))

; -- Hacking --

(defn exploit-edit
	"Crack a ciphertext given an editor by exploiting the invertibility of xor"
	[cipher editor]
	(->
		cipher
		(editor 0 (repeat (count cipher) 0))
		(xor/xor cipher)))

(defn- find-insertion-point [encoder]
	(->>
		[0 1]
		(map #(count (take-while true? (map = (encoder []) (encoder [%])))))
		(apply min)))

(defn exploit-userdata [encoder insertion]
	(let [n (find-insertion-point encoder)]
		(->
			insertion
			count
			(repeat 0)
			encoder
			(xor/xor-mid n insertion))))

; -- Solvers (problems) --

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

(defn solve-exploit-edit [file]
	(let [key (aes/rand-key)
				plain (->> file util/get-lines (join "\n") util/byte-string)
				cipher (encrypt key plain)
				editr (editor key)
				hacked (exploit-edit cipher editr)]
		(util/char-string hacked)))