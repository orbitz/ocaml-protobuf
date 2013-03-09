open Core.Std

type t = Int64.t

type error = [ `Incomplete | `Overflow ]

let encode t =
  Int64.(bit_xor
	   (shift_left t 1)
	   (shift_right t 63))

let decode t =
  Int64.(bit_xor
	   (shift_right t 1)
	   (neg (bit_and t one)))

let of_bitstring bits =
  let open Result.Monad_infix in
  Varint.of_bitstring bits >>= fun (i, rest) ->
  Ok (encode i, rest)

let to_string t = failwith "nyi"
