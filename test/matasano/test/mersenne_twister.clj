(ns matasano.test.mersenne-twister
	(:use [clojure.test])
  (:require [matasano.util :as util])
  (:require [matasano.mersenne-twister :as mt]))

(defn print-ret [x]
	(printf "%08x\n" x)
	x)

(deftest basic-vector
	(is (util/map= [2991312382, 3062119789, 1228959102]
				(mt/stream 123))))

(deftest untemper
	(is (every? #(= % (-> % mt/temper mt/untemper)) (repeatedly 20 #(rand-int Integer/MAX_VALUE)))))

(deftest untemper-foreal
	(let [[fv instance] (mt/next-value (mt/make-instance (rand-int Integer/MAX_VALUE)))]
		(is (= 1 (instance :index)))
		(is (= (first (instance :mt)) (mt/untemper fv)))
		(is (util/map= (util/slice 1 5 (instance :mt)) (take 5 (map mt/untemper (mt/instance-stream instance)))))))

(deftest stream
	(let [strm (mt/stream (rand-int Integer/MAX_VALUE))]
		(is (util/map= (take 625 strm) (take 625 strm)))))

(deftest clone
	(let [strm (mt/stream 123456789)
				cln (mt/clone-stream strm)]
		(is (util/map= (take 1000 strm) cln))))