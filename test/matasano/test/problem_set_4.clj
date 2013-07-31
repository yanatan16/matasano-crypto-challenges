(ns matasano.test.problem-set-4
	(:require [matasano.util :as util])
	(:use [clojure.string :only (join)])
	(:require [matasano.aes-ctr :as aes-ctr])
  (:use [clojure.test]))

; Problem 25
(deftest problem-twenty-five
	(is (util/map=
		(aes-ctr/solve-exploit-edit "prob25-input.txt")
		(->> "prob25-input.txt" util/get-lines (join "\n")))))