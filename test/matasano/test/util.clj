(ns matasano.test.util
	(:require [matasano.util :as util])
  (:use [clojure.test]))

(deftest test-map=
	(is (util/map= (list 1 2 3 4) [1 2 3 4])))

(deftest test-byte-string
	(is (util/map= (byte-array [(byte 1) (byte 2)]) (util/true-byte-string [1 2]))))

(deftest test-rotate
	(is (util/map= [0 1 2 3 4 5 6 7 8 9] (util/rotate (range 10) 0)))
	(is (util/map= [1 2 3 4 5 6 7 8 9 0] (util/rotate (range 10) 11)))
	(is (util/map= [6 7 8 9 0 1 2 3 4 5] (util/rotate (range 10) 6))))

(deftest test-char-string
	(is (= "hey there" (util/char-string (util/byte-string "hey there"))))
	(is (= "yoyo" (util/char-string [121 111] [121 111]))))

(deftest int-byte
	(is (util/map= [0 0 0 0 0 0 0 0] (util/int-bigend-64 0)))
	(is (util/map= [0 0 0 1 0x23 0x45 0x67 0x89] (util/int-bigend-64 4886718345)))
	(is (util/map= [0 0 0 0 0 0 0 0] (util/int-lilend-64 0)))
	(is (util/map= [0x89 0x67 0x45 0x23 1 0 0 0  ] (util/int-lilend-64 4886718345))))

(deftest long-string
	(is (util/map= [0xabcdef12 0x34567890] (util/long-string [0xab 0xcd 0xef 0x12 0x34 0x56 0x78 0x90]))))

(deftest splice
	(is (= "hellomyangelhello" (apply str (util/splice 5 "hello how i hello" "myangel"))))
	(is (= "hellomyangel how i hello" (apply str (util/splice 5 0 "hello how i hello" "myangel")))))