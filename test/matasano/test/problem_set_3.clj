(ns matasano.test.problem-set-3
	(:require [matasano.util :as util])
	(:require [matasano.aes :as aes])
	(:require [matasano.attack-aes-cbc :as attack-aes-cbc])
	(:require [matasano.aes-ctr :as aes-ctr])
  (:use [clojure.test]))

(def plain-text-17
	(map util/unbase64ify (util/get-lines "prob17-input.txt")))

(deftest problem-seventeen
	(let [iv (aes/rand-key)
				key (aes/rand-key)
				checker (attack-aes-cbc/make-padding-checker iv key)]
		(doall (map (fn [plain-text]
			(is (= plain-text
				(attack-aes-cbc/hack-cbc-lookup-by-pad plain-text-17 iv checker
					(aes/cbc-encrypt iv key (aes/pkcs7-pad 16 plain-text))))))
			plain-text-17))))

(deftest problem-eighteen
	(->>
		"L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
		util/unbase64ify
		(aes-ctr/decrypt (util/byte-string "YELLOW SUBMARINE") 0)
		util/char-string
		(= "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby ")
		is))

; problem 18 by hand
; keystream:
; (115 -75 -29 118 -98 -26 40 123 -63 -94 62 41 -50 -54 -39 114 -40 100 -73 62 53 91 120 57 7 -6 -29 48 34 -67 69 -75 -42 61 -118 -45 -97 8)

; I have met them at close of day
; Coming with vivid faces
; From counter or desk among grey
; Eighteenth-century houses.
; I have passed with a nod of the head
; Or polite meaningless words,
; Or have lingered awhile and said
; Polite meaningless words,
; And thought before I had done
; Of a mocking tale or a gibe
; To please a companion
; Around the fire at the club,
; Being certain that they and I
; But lived where motley is worn:
; All changed, changed utterly:
; A terrible beauty is born.

; That woman's days were spent
; In ignorant good-will,
; Her nights in argument
; Until her voice grew shrill.
; What voice more sweet than hers
; When, young and beautiful,
; She rode to harriers?
; This man had kept a school
; And rode our wingèd horse;
; This other his helper and friend
; Was coming into his force;
; He might have won fame in the end,
; So sensitive his nature seemed,
; So daring and sweet his thought.
; This other man I had dreamed
; A drunken, vainglorious lout.
; He had done most bitter wrong
; To some who are near my heart,
; Yet I number him in the song;
; He, too, has resigned his part
; In the casual comedy;
; He, too, has been changed in his turn,
; Transformed utterly:
; A terrible beauty is born.


; Problem 20
; Solved with `lein run 20 prob20-input.txt`
; Cleartext: Rakim's Rated "R" and "Paid in Full" full lyrics
(deftest problem-twenty
	(is (util/map= (util/get-lines "prob20-output.txt") (aes-ctr/solve-crypt-multi "prob20-input.txt"))))