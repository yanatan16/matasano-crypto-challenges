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

(defn- y-val [y]
	(reduce #(bit-xor %1 (%2 %1)) y [
			#(bit-shift-right % 11)
			#(bit-and (bit-shift-left % 7) 0x9d2c5680)
			#(bit-and (bit-shift-left % 15) 0xefc60000)
			#(bit-shift-right % 18)
		]))

(defn- generate-numbers [mt]
	(map
		(fn [i]
			(let [y (+ (bit-and (nth mt i) 0x80000000) (bit-and (nth mt (mod (inc i) 624)) 0x7fffffff))
						xval (if (odd? y) 2567483615 0)]
				(bit-xor (nth mt (mod (+ i 397) 624)) (bit-shift-right y 1) xval)))
		(range 624)))

(defn next-value [instance]
	(let [mt (if (zero? (instance :index)) (generate-numbers (instance :mt)) (instance :mt))]
		[
			(y-val (nth mt (instance :index)))
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