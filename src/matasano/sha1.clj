(ns matasano.sha1
	(:use clojure.contrib.core)
	(:require [matasano.util :as util])
	(:import com.matasano.SHA1Two))

(defn sha1 [text]
	(SHA1Two/hash text))



(defn reverse-instance
	([digest len] (new SHA1 digest len))
	([digest pos len] (new SHA1 digest len pos))
	([digest] (reverse-instance digest 0)))

(defn update [instance & texts]
	(doall (map
		(fn [x]
			(.update instance (util/true-byte-string x)))
		texts))
	instance)

(defn digest [instance]
	(.complete instance)
	(.digest instance))

(defn sha1 [& texts]
	(->
		(instance)
		(#(apply update % texts))
		digest))

(defn pad
	([text] (pad text (count text)))
	([text len]
		(->
			text
			(concat [0x80])
			(concat (repeat (mod (- 55 len) 64) 0))
			(concat (util/int-bigend-64 (* len 8))))))

; -- Exploits --

(defn- pad-appendage [text len append]
	(concat (pad text len) append))

(defn- extend-mac [mac append]
	(->
		mac
		reverse-instance
		(update append)
		digest))

(defn length-extension [text mac append len]
	[
		(pad-appendage text len append)
		(extend-mac mac append)
	])

(defn expoit-length-extension [text mac append checker]
	(-?>>
		(range 100)
		(length-extension text mac append)
		(filter checker)
		first))