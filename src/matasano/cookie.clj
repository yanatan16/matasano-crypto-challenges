(ns matasano.cookie
	(:require [clojure.string :as string])
	(:require [matasano.util :as util])
	(:require [matasano.aes :as aes]))

(defn url-decode [encoded]
	(apply hash-map (string/split encoded #"[&=]")))

(defn url-encode [obj]
	(string/join "&"
		(map (fn [[k v]] (str k "=" v)) obj)))

(defn sanitize [s]
	(->
		s
		util/char-string
		(string/escape { \& "" \= "" })))

(defn profile-for
	([email] (profile-for email "user"))
	([email role]	(url-encode (array-map
			"email" (sanitize email)
			"uid" 10
			"role" (sanitize role)
		))))

(defn make-userdata-encoder [encryptor pre post]
  (fn [user-data]
    (encryptor (apply concat (map util/byte-string [pre (sanitize user-data) post])))))

(defn make-userdata-checker [decryptor check-for]
  (fn [encoded-user-data]
    (let [decoded (util/char-string (decryptor encoded-user-data))]
      (some
        #(= check-for %)
        (string/split decoded #";")))))

(defn encrypt-profile [key & profile-args]
	(aes/encrypt key (apply profile-for profile-args)))

(defn decrypt-profile [key cipher]
	(url-decode (util/char-string (aes/pkcs7-unpad (aes/decrypt key cipher)))))

(defn create-oracle-profile [key]
	(partial encrypt-profile key))

(defn craft-arbitrary-cipher
	"Use the encrypt-profile oracle and the email input to craft cipher text just for the input"
	[profile-oracle plain]
	(let [padded (aes/pkcs7-pad 16 plain)
				input (util/char-string "iamf@ke.it" padded)]
		;The 2nd+ encrypted blocks will be just the input text provided and pad
		(util/slice 16 (count padded) (profile-oracle input))))

(defn known-plaintext-replace-attack
	[oracle plain-text cipher-text re-replace replace-to]
	(let [location (count (first (string/split plain-text re-replace)))
				block-loc (- location (mod location 16))
				post (apply str (nthrest plain-text block-loc))]
		(util/true-byte-string
			(concat
				(take block-loc cipher-text)
				(craft-arbitrary-cipher oracle
					(string/replace post re-replace replace-to))))))

; We know the plaintext that will be encrypted using ecb.
; We simply have to calculate the last plaintext block (with pad)
;  replace "user" for "admin" and then craft an email input to calculate that block(s)
(defn hack-admin-profile
	"Take an email and use the encrypt-profile oracle to craft an admin account."
	[profile-oracle email] ; The email must be a correct length or this will fail
	(known-plaintext-replace-attack
		profile-oracle
		(profile-for email)
		(profile-oracle email)
		#"user"
		"admin"))

(defn craft-email [domain]
	(let [base-length (count (profile-for domain))
				pad (- 16 (mod (- base-length 4) 16))]
		(str (apply str (repeat pad \a)) domain)))
