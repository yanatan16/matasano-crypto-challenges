(ns matasano.hash
	(:require [matasano.util :as util]))


(defn mac [hasher key message]
	(hasher (concat key message)))

(defn mac-verify [hasher key message sig]
	(util/map= sig (mac hasher key message)))
