(ns matasano.test.attack-aes
  (:use [matasano.cryptotools])
  (:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.aes :as aes])
  (:require [matasano.attack-aes :as attack-aes]))

(def ex-key (aes/rand-key))
(def ex-oracle1 (attack-aes/create-oracle-ecb ex-key "Jon Stewart is on comedy central."))
(def ex-oracle (attack-aes/create-oracle-ecb-2 ex-key (util/rand-bytes 5) "Jon Stewart is on comedy central."))

(deftest guess-block-size
	(is (= 16 (attack-aes/guess-block-size ex-oracle))))

(deftest guess-prepend-size
	(doall (map
		(fn [n]
			(is (= n (attack-aes/guess-prepend-size
				(attack-aes/create-oracle-ecb-2 ex-key (util/rand-bytes n) "Jon Stewart is on comedy central.")
				16))))
		(range 0 32 4))))

(deftest wrap-oracle-prepend
	(let [wrapped (attack-aes/wrap-oracle-prepend ex-oracle 5 16)]
		(is (= (count (ex-oracle [])) (count (wrapped []))))
		(is (util/map= (ex-oracle1 "HELLO CHARLIE") (wrapped "HELLO CHARLIE")))
		(is (= (count "Jon Stewart is on comedy central.")
			(attack-aes/guess-input-length ex-oracle1 16) (attack-aes/guess-input-length wrapped 16)))))