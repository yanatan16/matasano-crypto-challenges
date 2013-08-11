(ns matasano.test.sha1
	(:use clojure.test)
	(:use matasano.sha1)
	(:require [matasano.util :as util]))

(deftest basic-tests
	(is (util/map=
		(util/unhexify "84983E441C3BD26EBAAE4AA1F95129E5E54670F1")
		(map util/un-ubyte (sha1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

	(is (util/map=
		(util/unhexify "A9993E364706816ABA3E25717850C26C9CD0D89D")
		(map util/un-ubyte (sha1 "abc"))))
	)