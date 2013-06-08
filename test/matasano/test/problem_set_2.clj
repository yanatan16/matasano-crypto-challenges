(ns matasano.test.problem-set-2
	(:require [matasano.util :as util])
	(:require [matasano.aes :as aes])
	(:require [matasano.attack-aes :as attack-aes])
	(:require [matasano.test.problem-set-1 :as test-ps1])
	(:require [matasano.cookie :as cookie])
  (:use [clojure.test]))

(def yellow-submarine (util/byte-string "YELLOW SUBMARINE"))
(def encrypted-input "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")

(deftest problem-nine
	(is (= yellow-submarine (aes/pkcs7-pad 16 yellow-submarine)))
	(is (= (concat yellow-submarine [4 4 4 4]) (aes/pkcs7-pad 20 yellow-submarine)))
	(is (= [1 2 3 4 5 6 7 8 9 0 6 6 6 6 6 6] (aes/pkcs7-pad 8 [1 2 3 4 5 6 7 8 9 0]))))

(deftest problem-ten
	(is (util/map=
		test-ps1/funky-music
		(aes/solve-cbc-decrypt "YELLOW SUBMARINE" "prob10-input.txt"))))

(deftest problem-eleven
	(doall (repeatedly 4 #(is (attack-aes/guess-cbc)))))

(deftest problem-twelve
	(is (util/map=
		"Rollin' in my 5.0\nWith my rag-top down so my hair can blow\nThe girlies on standby waving just to say hi\nDid you stop? No, I just drove by\n"
		(attack-aes/solve-break-oracle-ecb encrypted-input))))

(deftest problem-thirteen
	(let [key (aes/rand-key)
				email (cookie/craft-email "@jon.com")]
		(is (= (cookie/url-decode (cookie/profile-for email "admin"))
			(cookie/decrypt-profile key
				(cookie/hack-admin-profile
					(cookie/create-oracle-profile key)
					email))))))

(deftest problem-fourteen
	(is (util/map=
		"Rollin' in my 5.0\nWith my rag-top down so my hair can blow\nThe girlies on standby waving just to say hi\nDid you stop? No, I just drove by\n"
		(attack-aes/solve-break-oracle-ecb-2 encrypted-input))))