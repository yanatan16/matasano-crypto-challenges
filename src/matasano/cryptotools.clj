(ns matasano.cryptotools
	(:require [clojure.contrib.combinatorics :as combo]))

(defn hamming-weight [byte]
	(apply +
		(map #(if (zero? (bit-and (bit-shift-left 1 %) byte)) 0 1) (range 8))))

(defn hamming-distance [a b]
	(apply + (map (comp hamming-weight bit-xor) a b)))

(defn deep-equal [a b]
	(if (seq? a)
		(every? true? (map deep-equal a b))
		(= a b)))

(defn mean-pairwise [func & args]
	(let [pairs (filter (complement #(apply deep-equal %)) (combo/cartesian-product args args))]
		(/
			(apply +
				(map #(apply func %) pairs))
			(count pairs))))