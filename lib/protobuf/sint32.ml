open Core.Std

type t = Int32.t

type error = [ `Incomplete | `Overflow ]

let encode t =
  Int32.(bit_xor
	   (shift_left t 1)
	   (shift_right t 31))

let decode t =
  Int32.(bit_xor
	   (shift_right t 1)
	   (neg (bit_and t one)))

let of_bitstring bits =
  let open Result.Monad_infix in
  Varint.of_bitstring bits >>= fun (i, rest) ->
  match Int32.of_int64 i with
    | Some i ->
      Ok (encode i, rest)
    | None ->
      Error `Overflow

let to_string t = failwith "nyi"
