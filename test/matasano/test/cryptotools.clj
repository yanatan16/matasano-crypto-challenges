(ns matasano.test.cryptotools
  (:use [matasano.cryptotools])
  (:use [clojure.test])
  (:require [matasano.util :as util]))

(deftest test-hamming-weight
	(is (= 8 (hamming-weight 0xff)))
	(is (= 3 (hamming-weight 0x07)))
	(is (= 0 (hamming-weight 0x00))))

(deftest test-hamming-distance
  (is
  	(= 37 (apply hamming-distance (map util/byte-string ["this is a test" "wokka wokka!!!"])))
  	"Hamming distance is incorrect!?"))

(deftest test-mean-pairwise
	(is
		(= 26/3 (apply mean-pairwise hamming-distance (map util/byte-string ["abc" "123" "456"])))
		"Mean Pairwise Hamming Distance incorrect")
	(is (= 8 (mean-pairwise + 3 4 7 2))))