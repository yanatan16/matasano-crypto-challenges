(ns matasano.aes-ctr
	(:require [matasano.util :as util])
  (:require [matasano.aes :as aes])
  (:require [matasano.fixedxor :as xor]))

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