(ns matasano.test.mersenne-twister
	(:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.mersenne-twister :as mt]))

(deftest basic-vector
	(is (util/map= [2991312382, 3062119789, 1228959102]
				(mt/stream 123))))

(deftest untemper
	(is (every? #(-> % mt/temper mt/untemper) (repeatedly 20 #(rand-int Integer/MAX_VALUE)))))

; (deftest test-untemper
; 	(is (-> 0xab8235d1 print-temper print-untemper)))