(ns matasano.test.problem-set-3
	(:require [matasano.util :as util])
	(:require [matasano.aes :as aes])
	(:require [matasano.attack-aes-cbc :as attack-aes-cbc])
	(:require [matasano.aes-ctr :as aes-ctr])
  (:use [clojure.test]))

(def plain-text-17
	(map util/unbase64ify [
		"MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
		"MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
		"MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
		"MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
		"MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
		"MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
		"MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
		"MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
		"MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
		"MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]))

(deftest problem-seventeen
	(let [iv (aes/rand-key)
				key (aes/rand-key)
				checker (attack-aes-cbc/make-padding-checker iv key)]
		(doall (map (fn [plain-text]
			(is (= plain-text
				(attack-aes-cbc/hack-cbc-lookup-by-pad plain-text-17 iv checker
					(aes/cbc-encrypt iv key (aes/pkcs7-pad 16 plain-text))))))
			plain-text-17))))

(deftest problem-eighteen
	(->>
		"L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
		util/unbase64ify
		(aes-ctr/decrypt (util/byte-string "YELLOW SUBMARINE") 0)
		util/char-string
		(= "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby ")
		is))