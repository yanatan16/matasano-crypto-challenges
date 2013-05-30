(ns matasano.test.aes
  (:use [matasano.cryptotools])
  (:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.aes :as aes]))

(deftest ebc-encyrption
	(let [key (util/rand-bytes 16)
				plain (util/true-byte-string "I HAS SUPERMAN. YOYOS ARE GLOWIN")]
		(is (util/map= plain (aes/decrypt key (aes/encrypt key plain))))))

(deftest cbc-encryption
	(let [iv (util/rand-bytes 16)
				key (util/rand-bytes 16)
				plain (util/true-byte-string "YELLOW SUBMARINEYELLOW SUBMARINE")
				cipher (aes/cbc-encrypt iv key plain)]
		(is (util/map= plain (aes/cbc-decrypt iv key cipher)))))