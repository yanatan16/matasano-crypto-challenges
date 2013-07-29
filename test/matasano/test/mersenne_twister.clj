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

(deftest reverse-state
	(let [instance (mt/make-instance 123)
				[fv ninst] (mt/next-value instance)
				orig-state (mt/reverse-state 624 (ninst :mt))]
		(is (util/map= (rest (instance :mt)) (rest orig-state)))))

(deftest reverse-state-part
	(let [inst (mt/make-instance 789)
				[_ ninst] (mt/next-value inst)
				strm (nthrest (mt/instance-stream inst) 25)
				utemp (apply vector (map mt/untemper (take 624 strm)))
				clnmt (concat (nthrest utemp 599) (take 599 utemp))
				orig-state (mt/reverse-state 25 clnmt)]
		(is (util/map= (rest (ninst :mt)) (rest orig-state)))))

(deftest clone-later
	(let [strm (nthrest (mt/stream 123456789) 25)
				cln (mt/clone-stream strm 25)]
		(is (util/map= (take 700 strm) cln))))

(deftest hack-seed
	(let [inst (mt/make-instance 0xabcdef)]
		(is (= 0xabcdef (mt/hack-seed (second (inst :mt)))))))

(deftest cipher
	(let [key (rand-int 0xffff)
				plain (util/byte-string "abcdefghijklmnopqrstuvwxyz")]
		(is (util/map= plain (mt/decrypt key (mt/encrypt key plain))))))