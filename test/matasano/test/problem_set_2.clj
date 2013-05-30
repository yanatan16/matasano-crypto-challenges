(ns matasano.test.problem-set-2
	(:require [matasano.util :as util])
	(:require [matasano.aes :as aes])
	(:require [matasano.test.problem-set-1 :as test-ps1])
  (:use [clojure.test]))

(def yellow-submarine (util/byte-string "YELLOW SUBMARINE"))

(deftest problem-nine
	(is (= yellow-submarine (aes/pkcs7-pad 16 yellow-submarine)))
	(is (= (concat yellow-submarine [4 4 4 4]) (aes/pkcs7-pad 20 yellow-submarine)))
	(is (= [1 2 3 4 5 6 7 8 9 0 6 6 6 6 6 6] (aes/pkcs7-pad 8 [1 2 3 4 5 6 7 8 9 0]))))

(deftest problem-ten
	(is (util/map=
		test-ps1/funky-music
		(aes/solve-cbc-decrypt "YELLOW SUBMARINE" "prob10-input.txt"))))

(deftest problem-eleven
	(doall (repeatedly 4 #(is (aes/guess-cbc)))))