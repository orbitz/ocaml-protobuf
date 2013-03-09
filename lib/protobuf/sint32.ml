open Core.Std

type t = Int32.t

let encode t =
  Int32.(bit_xor
	   (shift_left t 1)
	   (shift_right t 31))

let decode t =
  Int32.(bit_xor
	   (shift_right t 1)
	   (neg (bit_and t one)))
