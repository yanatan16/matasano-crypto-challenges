(ns matasano.mersenne-twister
	(:require [matasano.util :as util])
	(:require [matasano.stream :as stream]))

(defn- init-state-helper [n i]
	(->
		n
		(bit-shift-right 30)
		(bit-xor n)
		(* 1812433253)
		(+ i)
		(bit-and 0xffffffff)))

(defn- init-state [seed]
	(apply vector
		(reduce
			(fn [l i]
				(conj l (init-state-helper (last l) i)))
			[seed]
			(range 1 624))))

(defn make-instance [seed]
	{
		:index 624
		:mt (init-state seed)
	})

(defn temper [y]
	(reduce #(bit-xor %1 (%2 %1)) y [
			#(bit-shift-right % 11)
			#(bit-and (bit-shift-left % 7) 0x9d2c5680)
			#(bit-and (bit-shift-left % 15) 0xefc60000)
			#(bit-shift-right % 18)
		]))

(defn- generate-numbers [mt]
	(reduce
		(fn [state i]
			(let [y (+
								(bit-and (state i) 0x80000000)
								(bit-and (state (mod (inc i) 624)) 0x7fffffff))
						xval (if (odd? y) 2567483615 0)
						result (bit-xor
							(state (mod (+ i 397) 624))
							(bit-shift-right y 1)
							xval)]
				(assoc state i result)))
		(apply vector mt)
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

(def crypt-instance (stream/encrypt (stream/long-stream next-value)))
(defn encrypt [key plain]
	(crypt-instance (make-instance key) plain))
(def decrypt encrypt)

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

(defn- first-bit? [n]
	(->
		n
		(bit-and 0x80000000)
		zero?
		not))

(defn reverse-state [index mt]
	(apply vector
		(reduce
			(fn [state i]
				(let [y (bit-xor (state i) (state (mod (+ i 397) 624)))
							y2 (if (first-bit? y) (bit-xor y 0x9908b0df) y)
							z (bit-xor (state (mod (+ i 623) 624)) (state (mod (+ i 396) 624)))
							z2 (if (first-bit? z) (bit-xor z 0x9908b0df) z)
							result (+
								(-> y2 (bit-shift-left 1) (bit-and 0x80000000))
								(-> z2 (bit-shift-left 1) (bit-and 0x7fffffff))
								(if (first-bit? z) 1 0))]
					(assoc state i result)))
			(apply vector mt)
			(reverse (range index)))))

(defn hack-seed [sec]
	(let [tmp (- sec 1)
				tmp2 (bit-and (* tmp -1774682003) 0xffffffff)]
		(bit-xor tmp2 (bit-shift-right tmp2 30))))

(defn make-instance-from-state [index mt]
	; (println "make-inst" (take 5 mt))
	{
		:index index
		:mt (reverse-state index (concat (nthrest mt (- 624 index)) (take (- 624 index) mt)))
	})

(defn clone-instance
	([strm] (clone-instance strm 0))
	([strm index]
		(->>
			strm
			(map untemper)
			(take 624)
			(make-instance-from-state index))))

(defn clone-stream
	([strm] (clone-stream strm 0))
	([strm index]
		(->
			strm
			(clone-instance index)
			instance-stream)))

(defn find-recent-seed
	"Find the seed of a recently (1000 s) seeded Mersenne Twister instance using its first value"
	[fv]
	(let [now (util/unix)
				trange (range (- now 1000) (+ now 1))
				rmap (zipmap (map #(-> % stream first) trange) trange)]
		(rmap fv)))