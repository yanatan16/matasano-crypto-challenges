(ns matasano.test.aes
  (:use [matasano.cryptotools])
  (:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.aes :as aes]))

(deftest ebc-encryption
	(let [key (util/rand-bytes 16)
				plain (util/true-byte-string "I HAS SUPERMAN. YOYOS ARE GLOWIN")]
		(is (util/map= (util/char-string plain)
			(util/char-string
				(aes/decrypt key
					(aes/encrypt key plain)))))))

(deftest cbc-encryption
	(let [iv (util/rand-bytes 16)
				key (util/rand-bytes 16)
				plain (util/true-byte-string "YELLOW SUBMARINEYELLOW SUBMARINE")]
		(is
			(util/map= (util/byte-string plain)
				(->>
					(aes/cbc-encrypt iv key plain)
					(aes/cbc-decrypt iv key)
					(util/byte-string))))))

(deftest test-padding
	(is (util/map= [1 2 3 5 5 5 5 5] (aes/pkcs7-pad 8 [1 2 3])))
	(is (util/map= [1 2 3] (aes/pkcs7-unpad (aes/pkcs7-pad 8 [1 2 3]))))
	(is (util/map= [1 2 1 2 4 4 4 4] (aes/pkcs7-pad 4 [1 2 1 2])))
	(is (util/map= [112 111 105 110 116 44 32 110 111 32 102 97 107 105 110 103]
		(aes/pkcs7-unpad (aes/pkcs7-pad [112 111 105 110 116 44 32 110 111 32 102 97 107 105 110 103])))))