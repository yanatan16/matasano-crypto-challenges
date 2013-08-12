(ns matasano.sha1
	(:require [matasano.util :as util])
	(:import com.matasano.SHA1))

(defn sha1 [& texts]
	(let [instance (new SHA1)]
		(doall (map #(.update instance (util/true-byte-string %)) texts))
		(.digest instance)))

(defn pad [text]
	(->
		text
		(concat [0x80])
		(concat (repeat (mod (- 55 (count text)) 64) 0))
		(concat (util/int-bigend-64 (* (count text) 8)))))