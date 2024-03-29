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

(defn pkcs7-pad
  ([factor bytes]
    (let [pad (+ (mod (- 0 (count bytes) 1) factor) 1)]
      (concat bytes (repeat pad pad))))
  ([bytes] (pkcs7-pad 16 bytes)))

(defn pkcs7-unpad [bytes]
  (let [b (last bytes)
        pad (take b (reverse bytes))]
    (if (every? (partial = b) pad)
      (reverse (drop-while (partial = b) (reverse bytes)))
      (throw (Throwable. "Bytestring is not pkcs7-padded!")))))

(defn raw-encrypt
  [key s]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/NoPadding")
                 (.init Cipher/ENCRYPT_MODE (secret key)))]
    (util/true-byte-string
      (.doFinal cipher (util/true-byte-string s)))))

(defn encrypt [key s] (raw-encrypt key (pkcs7-pad 16 s)))

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
          (let [next-cipher (util/byte-string (raw-encrypt key (xor/xor avec next-plain)))]
            (list (concat cipher next-cipher) next-cipher)))
        (list [] iv)
        (partition keysize (util/byte-string plain))))))

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
	[key file]
  (let [text (util/unbase64ify (apply str (util/get-lines file)))]
  	(util/char-string
  		(decrypt key
  			(byte-array (map byte text))))))

(defn solve-cbc-decrypt
  [key file]
  (->>
    file
    util/get-lines
    (map util/unbase64ify)
    (apply concat)
    (cbc-decrypt (util/true-byte-string (repeat 16 0)) key)
    util/char-string))