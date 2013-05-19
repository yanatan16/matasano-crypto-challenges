(ns matasano.base64
	(:require [matasano.util :as util]))

(defn hexto64 [obj]
	(util/base64ify
		(util/unhexify obj)))

(defn solve
	"De-hex and encode base64 a string"
	[val & more]
  (hexto64 val))