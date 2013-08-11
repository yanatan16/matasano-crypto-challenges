(ns matasano.attack-aes
  (:use matasano.aes)
	(:require [matasano.util :as util])
  (:require [clojure.contrib.math :as math])
  (:require [matasano.cookie :as cookie]))

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

(defn create-oracle-ecb-2 [key random-prepend random-input]
  (comp (partial encrypt key) #(concat random-prepend % random-input)))

(defn create-oracle-cbc-2 [iv key random-prepend random-input]
  (comp (partial cbc-encrypt iv key) #(concat random-prepend % random-input)))

(defn wrap-oracle-prepend [oracle prepend-size block-size]
  (let [pad (- block-size (mod prepend-size block-size))
        total (+ prepend-size pad)]
    (fn [input]
      (nthrest (oracle (concat (repeat pad \A) input)) total))))

(defn guess-input-length [oracle block-size]
  (let [base-length (count (oracle []))
        lengths (map #(list (count (oracle (repeat % \A))) %) (range (+ block-size 1)))
        [next-length pad] (first (filter #(not= (first %) base-length) lengths))]
    (- base-length pad)))

(defn guess-prepend-size [oracle block-size]
  (let [inputs (map #(repeat % \A) (range (+ 1 block-size)))
        ciphers (map #(partition 16 (oracle %)) inputs)
        diffs (map #(count (take-while true? (map = %1 %2))) ciphers (rest ciphers))]
    (reduce + diffs)))

(defn guess-block-size [oracle]
  (let [encrypts (map #(oracle (repeat % \A)) (range 1 257))
        equals (map #(count (take-while true? (map = %1 %2))) encrypts (rest encrypts))
        diffs (map - equals (rest equals))]
    (math/abs (first (drop-while #(= 0 %) diffs)))))

(defn nth-block [n size input] (util/slice (* size n) size input))

(defn guess-next-hidden-byte [oracle block-size known-guess]
  (let [input-block (repeat (- block-size 1 (mod (count known-guess) block-size)) (int \A))
        full-block (concat input-block known-guess)
        block (math/floor (/ (count full-block) block-size))
        get-block (partial nth-block block block-size)
        block-bytes (map (comp (partial concat full-block) list) (range 256))
        lookup (zipmap (map (comp get-block oracle) block-bytes) (range 256))]
    (lookup (get-block (oracle input-block)))))

(defn break-oracle-ecb [oracle block-size input-length]
  (let [guesser (partial guess-next-hidden-byte oracle block-size)]
    (reduce (fn [g _] (concat g [(guesser g)])) '() (range input-length))))

(defn break-oracle [oracle]
  (let [block-size (guess-block-size oracle)
        prepend-size (guess-prepend-size oracle block-size)
        oracle-prime (wrap-oracle-prepend oracle prepend-size block-size)
        is-ecb (ecb? oracle-prime)
        input-length (guess-input-length oracle-prime block-size)]
    (if is-ecb
      (break-oracle-ecb oracle-prime block-size input-length)
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
        (create-oracle-ecb key (util/unbase64ify input))))))

(defn solve-break-oracle-ecb-2
  [input]
  (let [key (rand-key)]
    (util/char-string
      (break-oracle
        (create-oracle-ecb-2 key (util/rand-bytes (+ 1 (rand-int 15))) (util/unbase64ify input))))))