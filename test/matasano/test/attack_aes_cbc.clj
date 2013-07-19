(ns matasano.test.attack-aes-cbc
  (:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.aes :as aes])
  (:require [matasano.attack-aes-cbc :as aacbc]))

(deftest test-hack-cbc-find-pad
	(let [iv (aes/rand-key)
				key (aes/rand-key)
				padcheck (aacbc/make-padding-checker iv key)]
	(is (= 5 (aacbc/hack-cbc-find-pad iv padcheck
		(aes/cbc-encrypt iv key (aes/pkcs7-pad "Hey baby, I wanna knowowowowo. Will you be ")))))))