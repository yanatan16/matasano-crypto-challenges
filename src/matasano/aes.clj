(ns matasano.aes
	(:require [matasano.util :as util])
  (:require [matasano.english :as english])
  (:require [matasano.fixedxor :as xor])
	(:import (javax.crypto Cipher))
  (:import (javax.crypto.spec SecretKeySpec))
  (:import (java.security Key)))

(defn- secret
  [s]
  (SecretKeySpec. (util/true-byte-string s) "AES"))

(defn pkcs7-pad [factor bytes]
  (let [pad (mod (- (count bytes)) factor)]
    (concat bytes (repeat pad pad))))

(defn pkcs7-unpad [bytes]
  (let [b (last bytes)
        pad (take b (reverse bytes))]
    (if (every? (partial = b) pad)
      (reverse (drop-while (partial = b) (reverse bytes)))
      bytes)))

(defn encrypt
  [key s]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/NoPadding")
                 (.init Cipher/ENCRYPT_MODE (secret key)))]
    (util/true-byte-string
      (.doFinal cipher (util/true-byte-string (pkcs7-pad 16 s))))))

(defn decrypt
  [key buf]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/NoPadding")
                 (.init Cipher/DECRYPT_MODE (secret key)))]
    (util/true-byte-string (.doFinal cipher buf))))

(defn rand-key [] (util/rand-bytes 16))

(defn cbc-encrypt [iv key plain]
  (let [keysize (count key)]
    (first
      (reduce
        (fn [[cipher avec] next-plain]
          (let [next-cipher (util/byte-string (encrypt key (xor/xor avec next-plain)))]
            (list (concat cipher next-cipher) next-cipher)))
        (list [] iv)
        (partition keysize (pkcs7-pad keysize plain))))))

(defn cbc-decrypt [iv key cipher]
  (let [keysize (count key)]
    (first
      (reduce
        (fn [[plain avec] next-cipher]
          (let [next-plain (xor/xor avec (util/byte-string (decrypt key (util/true-byte-string next-cipher))))]
            (list (concat plain next-plain) next-cipher)))
        (list [] iv)
        (partition keysize cipher)))))

(defn solve-decrypt
	[key file & more]
  (let [text (util/unbase64ify (apply str (util/get-lines file)))]
  	(util/char-string
  		(decrypt key
  			(byte-array (map byte text))))))

(defn solve-cbc-decrypt
  [key file & more]
  (let [text (apply concat (map util/unbase64ify (util/get-lines file)))
        iv (util/true-byte-string (repeat 16 0))]
    (util/char-string (cbc-decrypt iv key text))))