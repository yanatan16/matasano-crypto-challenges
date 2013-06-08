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