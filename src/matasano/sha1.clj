(ns matasano.sha1
	(:require [matasano.util :as util])
	(:import com.matasano.SHA1))

(defn sha1 [& texts]
	(let [instance (new SHA1)]
		(doall (map #(.update instance (util/true-byte-string %)) texts))
		(.digest instance)))