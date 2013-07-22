(ns matasano.stream
	(:use clojure.contrib.core)
	(:require [matasano.util :as util])
  (:require [matasano.fixedxor :as xor]))

(defn stream [get-block]
	(fn stream-inside [instance]
		(let [[b i] (get-block instance)]
			(concat b (lazy-seq (stream-inside i))))))

(defn encrypt [get-block]
	(fn [instance plain]
		(xor/xor plain ((stream get-block) instance))))