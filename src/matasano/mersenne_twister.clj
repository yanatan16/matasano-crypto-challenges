(ns matasano.mersenne-twister
	(:require [matasano.util :as util])
	(:require [matasano.stream :as stream]))

(defn lower-32-multiply [s m]
	(+
		(->
			s
			(bit-and 0xffff0000)
			(bit-shift-right 16)
			(* m)
			(bit-shift-left 16))
		(->
			s
			(bit-and 0x0000ffff)
			(* m))))

(defn make-instance [seed]
	{
		:index 0
		:mt (reduce
			(fn [l i]
				(conj l
					(->
						(last l)
						(bit-shift-right 30)
						(bit-xor (last l))
						(lower-32-multiply 1812433253)
						(+ i)
						(bit-and 0xffffffff))))
			[seed]
			(range 1 624))
	})

(defn temper [y]
	(reduce #(bit-xor %1 (%2 %1)) y [
			#(bit-shift-right % 11)
			#(bit-and (bit-shift-left % 7) 0x9d2c5680)
			#(bit-and (bit-shift-left % 15) 0xefc60000)
			#(bit-shift-right % 18)
		]))

(defn- generate-numbers [mt]
	(map
		(fn [i]
			(let [y (+
								(bit-and (nth mt i) 0x80000000)
								(bit-and (nth mt (mod (inc i) 624)) 0x7fffffff))
						xval (if (odd? y) 2567483615 0)]
				(bit-xor
					(nth mt (mod (+ i 397) 624))
					(bit-shift-right y 1)
					xval)))
		(range 624)))

(defn next-value [instance]
	(let [mt (if (zero? (instance :index)) (generate-numbers (instance :mt)) (instance :mt))]
		[
			(temper (nth mt (instance :index)))
			(assoc instance
				:mt mt
				:index (mod (inc (instance :index)) 624))
		]))

(def instance-stream (stream/value-stream next-value))

(defn stream [seed]
	(->
		seed
		make-instance
		instance-stream))

(defn untemper [y]
	(reduce #(bit-xor %1 (%2 %1)) y [
			#(bit-shift-right % 18)
			#(bit-and (bit-shift-left % 15) 0xefc60000)
			#(bit-and (bit-shift-left % 7) 0x00005680)
			#(bit-and (bit-shift-left % 7) 0x002c0000)
			#(bit-and (bit-shift-left % 7) 0x1d000000)
			#(bit-and (bit-shift-left % 7) 0x80000000)
			#(bit-and (bit-shift-right % 11) 0xffe00000)
			#(bit-and (bit-shift-right % 11) 0x001fc000)
			#(bit-and (bit-shift-right % 11) 0x00003f80)
			#(bit-and (bit-shift-right % 11) 0x0000007f)
		]))


(defn find-recent-seed
	"Find the seed of a recently (1000 s) seeded Mersenne Twister instance using its first value"
	[fv]
	(let [now (util/unix)
				trange (range (- now 1000) (+ now 1))
				rmap (zipmap (map #(-> % stream first) trange) trange)]
		(rmap fv)))