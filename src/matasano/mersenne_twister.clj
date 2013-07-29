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

(defn- init-state [seed]
	(reduce
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
		(range 1 624)))

(defn make-instance [seed]
	{
		:index 624
		:mt (init-state seed)
	})

(defn make-instance-from-state [mt]
	; (println "make-inst" (take 5 mt))
	{
		:index 0
		:mt mt
	})

(defn temper [y]
	(reduce #(bit-xor %1 (%2 %1)) y [
			#(bit-shift-right % 11)
			#(bit-and (bit-shift-left % 7) 0x9d2c5680)
			#(bit-and (bit-shift-left % 15) 0xefc60000)
			#(bit-shift-right % 18)
		]))

(defn- generate-numbers [mt]
	; (println "gen-nums" (take 5 mt))
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
	(let [mt (if (= 624 (instance :index)) (generate-numbers (instance :mt)) (instance :mt))]
		[
			(temper (nth mt (mod (instance :index) 624)))
			(assoc instance
				:mt mt
				:index (inc (mod (instance :index) 624)))
		]))

(def instance-stream (stream/value-stream next-value))

(defn stream [seed]
	(->
		seed
		make-instance
		instance-stream))

; ** Hacking section **

(defn- undo-bit-shift-mask [x n mask shifter indices]
	(reduce
		(fn [acc i]
			(->
				x
				(bit-xor acc)
				(bit-and (bit-shift-left 1 i))
				(shifter n)
				(bit-and mask)
				(bit-or acc)))
		0
		indices))

(defn- undo-bit-shift-left-mask [x n mask]
	(undo-bit-shift-mask x n mask bit-shift-left (range 0 (- 32 n))))

(defn- undo-bit-shift-right [x n]
	(undo-bit-shift-mask x n 0xffffffff bit-shift-right (range 31 (- n 1) -1)))

(defn untemper [y]
	(reduce #(bit-xor %1 (%2 %1)) y [
			#(undo-bit-shift-right % 18)
			#(undo-bit-shift-left-mask % 15 0xefc60000)
			#(undo-bit-shift-left-mask % 7 0x9d2c5680)
			#(undo-bit-shift-right % 11)
		]))

(defn clone-instance [strm]
	(->>
		strm
		(map untemper)
		(take 624)
		make-instance-from-state))

(defn clone-stream [strm]
	(->
		strm
		clone-instance
		instance-stream))

(defn find-recent-seed
	"Find the seed of a recently (1000 s) seeded Mersenne Twister instance using its first value"
	[fv]
	(let [now (util/unix)
				trange (range (- now 1000) (+ now 1))
				rmap (zipmap (map #(-> % stream first) trange) trange)]
		(rmap fv)))