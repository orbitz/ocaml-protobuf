open Core.Std

type t = Int64.t

type error = [ `Incomplete | `Overflow ]

let (<<<) l r = Int64.shift_left l r
let (+^)      = Int64.(+)
let i64       = Int64.of_int

let rec read_byte n acc bits =
  bitmatch bits with
    | { true  : 1
      ; b     : 7
      ; rest  : -1 : bitstring
      } ->
      read_byte (n + 1) (acc +^ (i64 b <<< (7 * n))) rest
    | { false : 1
      ; b     : 7
      ; rest  : -1 : bitstring
      } ->
      Ok (acc +^ (i64 b <<< (7 * n)), rest)
    | { _ } ->
      Error `Incomplete

let of_bitstring = read_byte 0 (i64 0)
let of_string s = of_bitstring (Bitstring.bitstring_of_string s)

let to_string t =
  let one_byte = Int64.of_int 127 in
  let b = Buffer.create 10 in
  let rec next_byte t =
    let byte =
      Int64.to_int_exn
	(Int64.bit_and one_byte t)
    in
    let t = Int64.shift_right t 7 in
    if Int64.equal t Int64.zero then
      Buffer.add_char b (Char.of_int_exn byte)
    else begin
      let byte = byte lor (1 lsl 7) in
      Buffer.add_char b (Char.of_int_exn byte);
      next_byte t
    end
  in
  next_byte t;
  Buffer.contents b

let to_bitstring t = Bitstring.bitstring_of_string (to_string t)
