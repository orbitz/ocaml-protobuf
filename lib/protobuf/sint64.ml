open Core.Std

type t = Int64.t

let encode t =
  Int64.(bit_xor
	   (shift_left t 1)
	   (shift_right t 63))

let decode t =
  Int64.(bit_xor
	   (shift_right t 1)
	   (neg (bit_and t one)))
