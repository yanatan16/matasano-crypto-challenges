(ns matasano.test.mersenne-twister
	(:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.mersenne-twister :as mt]))

(deftest basic-vector
	(is (util/map= [2991312382, 3062119789, 1228959102]
				(mt/stream 123))))