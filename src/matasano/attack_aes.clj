(ns matasano.attack-aes
  (:use matasano.aes)
	(:require [matasano.util :as util])
  (:require [matasano.english :as english])
  (:require [matasano.fixedxor :as xor])
  (:require [clojure.contrib.math :as math])
	(:import (javax.crypto Cipher))
  (:import (javax.crypto.spec SecretKeySpec))
  (:import (java.security Key)))

(defn count-offset-repeats
  "Count the byte repeats at some offset"
  [offset text]
  (let [columns (apply map vector (partition offset text))]
    (apply + (map #(- (count %) (count (distinct %))) columns))))

(defn ecb-score [text]
  (let [size (count text)]
  (- 1.0 (/ (count-offset-repeats 16 text) size))))

(defn oracle-cbc-detect [input]
  (let [plain (concat
          (util/rand-bytes (+ 5 (rand-int 5)))
          (util/byte-string input)
          (util/rand-bytes (+ 5 (rand-int 5))))
        encryptor (rand-nth (list encrypt (partial cbc-encrypt (rand-key))))]
    (list (encryptor (rand-key) (pkcs7-pad 16 plain)) (not= encryptor encrypt))))

(defn ecb? [oracle]
  (let [input (util/byte-string (repeat 100 \A))
        cipher (oracle input)]
    (< (ecb-score cipher) 0.8)))

(defn guess-cbc
  "Use the encrypt-oracle-cbc to guess cbc vs ecb"
  []
  (let [input (util/byte-string (repeat 100 \A))
        [cipher is-cbc] (oracle-cbc-detect input)
        guess-cbc (> (ecb-score cipher) 0.8)]
    (= guess-cbc is-cbc)))

(defn create-oracle-ecb [key random-input]
  (comp (partial encrypt key) #(concat % random-input)))

(defn guess-input-length [oracle block-size]
  (let [base-length (count (oracle []))
        lengths (map #(list (count (oracle (repeat % \A))) %) (range block-size))
        [next-length pad] (first (filter #(not= (first %) base-length) lengths))]
    (+ (- base-length pad) 1)))

(defn guess-block-size [encryptor oracle]
  (let [encrypts (map #(list (util/map=
                          (encryptor (repeat % \A))
                          (oracle (repeat % \A)))
                        %)
                      (range 1 257))]
    (second (first (filter first encrypts)))))

(defn nth-block [n size input] (util/slice (* size n) size input))

(defn guess-next-hidden-byte [encryptor oracle block-size known-guess]
  (let [bytes (range 256)
        input-block (repeat (- block-size 1 (mod (count known-guess) block-size)) (int \A))
        full-block (concat input-block known-guess)
        block (math/floor (/ (count full-block) block-size))
        get-block (partial nth-block block block-size)
        block-bytes (map (comp (partial concat full-block) list) bytes)
        lookup (zipmap (map (comp get-block encryptor) block-bytes) bytes)]
    (lookup (get-block (oracle input-block)))))

(defn break-oracle-ecb [encryptor oracle block-size input-length]
  (let [guesser (partial guess-next-hidden-byte encryptor oracle block-size)]
    (reduce (fn [g _] (concat g [(guesser g)])) '() (range input-length))))

(defn break-oracle [encryptor oracle]
  (let [block-size (guess-block-size encryptor oracle)
        is-ecb (ecb? oracle)
        input-length (guess-input-length oracle block-size)]
    (if is-ecb
      (break-oracle-ecb encryptor oracle block-size input-length)
      (println "No cbc break implemented!"))))

(defn solve-find-ecb
  [file & more]
  (let [texts (map util/unhexify (util/get-lines file))]
    (util/hexify (first (sort-by ecb-score texts)))))

(defn solve-break-oracle-ecb
  [input]
  (let [key (rand-key)]
    (util/char-string
      (break-oracle
        (partial encrypt key)
        (create-oracle-ecb key (util/unbase64ify input))))))