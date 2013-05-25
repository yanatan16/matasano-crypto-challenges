(ns matasano.aes
	(:require [matasano.util :as util])
  (:require [matasano.english :as english])
	(:import (javax.crypto Cipher))
  (:import (javax.crypto.spec SecretKeySpec))
  (:import (java.security Key)))

(defn- ^Key secret
  [^String s]
  (SecretKeySpec. (.getBytes s) "AES"))

(defn ^bytes encrypt
  [^String s ^String key]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/PKCS5Padding")
                 (.init Cipher/ENCRYPT_MODE (secret key)))]
    (.doFinal cipher (.getBytes s "UTF-8"))))

(defn ^String decrypt
  [^bytes buf ^String key]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/PKCS5Padding")
                 (.init Cipher/DECRYPT_MODE (secret key)))]
    (String. (.doFinal cipher buf) "UTF-8")))

(defn solve-decrypt
	[key file & more]
  (let [text (util/unbase64ify (apply str (util/get-lines file)))]
  	(util/char-string
  		(decrypt
  			(byte-array (map byte text))
        key))))