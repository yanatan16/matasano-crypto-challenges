(ns matasano.test.sha1
	(:use clojure.test)
	(:use matasano.sha1)
	(:use matasano.hash)
	(:require [matasano.fixedxor :as xor])
	(:require [matasano.util :as util]))

(deftest basic-tests
	(is (util/map=
		(util/unhexify "84983E441C3BD26EBAAE4AA1F95129E5E54670F1")
		(map util/un-ubyte (sha1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))
	(is (util/map=
		(util/unhexify "A9993E364706816ABA3E25717850C26C9CD0D89D")
		(map util/un-ubyte (sha1 "abc")))))

(deftest mactest
	(is (util/map=
		(util/unhexify "84983E441C3BD26EBAAE4AA1F95129E5E54670F1")
		(map util/un-ubyte (mac sha1 "abc" "dbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))
	(let [key "YELLOW SUBMARINE"
				message "I'm a man on a mission yo!"]
		(->>
			message
			(mac sha1 key)
			(mac-verify sha1 key message)
			is)))

(deftest tamper-test
	(let [key "YELLOW SUBMARINE"
				message "I'm a man on a mission yo!"]
		(is (false?
			(mac-verify sha1 key message
				(xor/xor (mac sha1 key message) (repeat 1)))))))

(deftest sha1-padding
	(is (util/map=
		[0x61 0x62 0x63 0x64  0x65 0x80 0 0  0 0 0 0  0 0 0 0
			0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
			0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
			0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0x28]
		(pad [0x61 0x62 0x63 0x64 0x65]))))