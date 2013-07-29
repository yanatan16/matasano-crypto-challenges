(ns matasano.stream
	(:use clojure.contrib.core)
	(:require [matasano.util :as util])
  (:require [matasano.fixedxor :as xor]))

(defn block-stream [get-block]
	(fn stream-inside [instance]
		(let [[b i] (get-block instance)]
			(concat b (lazy-seq (stream-inside i))))))

(defn value-stream [get-value]
	(fn stream-inside [instance]
		(let [[v i] (get-value instance)]
			(cons v (lazy-seq (stream-inside i))))))

(defn long-stream [get-long]
	(fn stream-inside [instance]
		(let [[v i] (get-long instance)]
			(concat (util/int-bigend-32 v) (lazy-seq (stream-inside i))))))

(defn encrypt [streamer]
	(fn [instance plain]
		(xor/xor plain (streamer instance))))

