(ns matasano.test.cookie
  (:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.cookie :as cookie])
  (:require [matasano.aes :as aes]))

(deftest test-url-encoding
	(is (= {"foo" "bar" "baz" "qux" "zap" "zazzle"}
		(cookie/url-decode "foo=bar&baz=qux&zap=zazzle")))
	(is (= "foo=bar&zap=zazzle&baz=qux"
		(cookie/url-encode {"foo" "bar" "baz" "qux" "zap" "zazzle"}))))

(deftest test-profile
	(let [obj (cookie/url-decode (cookie/profile-for "jon@jon&&=.com"))]
		(is (= (sorted-map "email" "jon@jon.com" "uid" "10" "role" "user") obj))))

(deftest test-profile-encryption
	(let [key (aes/rand-key)]
		(is
			(let [control	(cookie/url-decode (cookie/profile-for "jonjon@jonjon.jonjon" "admin"))
						tester (cookie/decrypt-profile key (cookie/encrypt-profile key "jonjon@jonjon.jonjon" "admin"))]
				(= control tester)))))

(deftest test-arbitrary-encryption
	(let [key (aes/rand-key)
				profile-oracle (cookie/create-oracle-profile key)]
		(is (util/map=
			(util/byte-string (aes/encrypt key "there's a snake in my boots man!"))
			(cookie/craft-arbitrary-cipher profile-oracle "there's a snake in my boots man!")))))