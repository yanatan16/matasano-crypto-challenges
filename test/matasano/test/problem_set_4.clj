(ns matasano.test.problem-set-4
	(:require [matasano.util :as util])
	(:use [clojure.string :only (join)])
	(:require [matasano.aes :as aes])
	(:require [matasano.aes-ctr :as aes-ctr])
	(:require [matasano.attack-aes-cbc :as attack-aes-cbc])
  (:use [clojure.test]))

; Problem 25
; Exploiting the "edit" function in AES/CTR
(deftest problem-twenty-five
	(is (util/map=
		(aes-ctr/solve-exploit-edit "prob25-input.txt")
		(->> "prob25-input.txt" util/get-lines (join "\n")))))

; Problem 26
; Known plaintext content changing in AES/CTR
(deftest problem-twenty-six
	(let [key (aes/rand-key)
				encoder (aes-ctr/make-userdata-encoder key
					"comment1=cooking%20MCs;userdata="
					";comment2=%20like%20a%20pound%20of%20bacon")
				checker (aes-ctr/make-userdata-checker key "admin=true")]
		(is (not (checker (encoder "admin=true;admin=true;admin=true"))))
		(is (checker (aes-ctr/exploit-userdata encoder (util/byte-string ";admin=true"))))))

; Problem 27
; same key and iv in AES/CBC
(deftest problem-twenty-seven
	(let [key (aes/rand-key)]
		(is (util/map= key
			(attack-aes-cbc/hack-same-key-iv
				(partial aes/cbc-encrypt key key)
				(partial aes/cbc-decrypt key key))))))