(ns matasano.attack-aes-cbc
  (:use matasano.aes)
  (:use matasano.attack-aes)
	(:require [matasano.util :as util])
  (:require [matasano.fixedxor :as xor])
  (:require [clojure.string :as string]))

(defn sanitize [s]
  (string/escape (util/char-string s) {
      \; ""
      \= ""
    }))

(defn make-userdata-encoder [iv key pre post]
  (fn [user-data]
    (cbc-encrypt iv key (apply concat (map util/byte-string [pre (sanitize user-data) post])))))

(defn make-userdata-checker [iv key check-for]
  (fn [encoded-user-data]
    (let [decoded (util/char-string (map util/un-ubyte (cbc-decrypt iv key encoded-user-data)))]
      (some
        #(= check-for %)
        (string/split decoded #";")))))

(defn make-padding-checker [iv key]
  (fn [cipher-text]
    (try
      (do
        (pkcs7-unpad (cbc-decrypt iv key cipher-text))
        true)
      (catch Throwable e false))))

(defn hack-create-sanitize-bit-errors
  "This creates a bit-flip on the lowest bits of the special characters"
  [s]
  (string/escape s {\; ":" \= "<"}))

(defn hack-bit-flip-ciphertext [encoded offset-prior offset to-add]
  (let [key (map #(if (#{\; \=} %) 1 0) to-add)]
    (concat
      (take offset-prior encoded)
      (xor/xor key (util/slice offset-prior (count key) encoded))
      (nthrest encoded (+ offset-prior (count key))))))

(defn hack-cbc-add-string [encoder to-add]
  (let [block-size (guess-block-size encoder)
        prepend-size (guess-prepend-size encoder block-size)
        pad (- (* 2 block-size) (mod prepend-size block-size))
        offset (+ prepend-size pad)
        hack-add (hack-create-sanitize-bit-errors to-add)
        encoded (encoder (concat (repeat pad \A) hack-add))]
    (hack-bit-flip-ciphertext encoded (- offset block-size) offset to-add)))

(defn hack-cbc-find-pad [iv pad-check cipher-text]
  (let [blocks (partition 16 cipher-text)
        second-to-last (last (butlast (concat [iv] blocks)))
        masks (map (fn [n] (map #(if (= n %) 1 0) (range 16))) (range 16))
        modified-2tls (map #(xor/xor second-to-last %) masks)
        modified-blocks (map #(concat blocks [% (last blocks)]) modified-2tls)
        pad-invalid (map #(if (pad-check (apply concat %)) 0 1) modified-blocks)]
    (reduce + pad-invalid)))

(defn hack-cbc-lookup-by-pad [possibles iv pad-check cipher-text]
  (let [pads (map pkcs7-pad possibles)
        lookup (zipmap (map #(list (count %) (last %)) pads) possibles)]
    (lookup (list (count cipher-text) (hack-cbc-find-pad iv pad-check cipher-text)))))