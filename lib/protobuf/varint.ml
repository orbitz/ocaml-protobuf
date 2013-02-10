open Core.Std

type t = Int64.t

type error = [ `Incomplete | `Overflow ]

let (<<<) l r = Int64.shift_left l r
let (+^)      = Int64.(+)
let i64       = Int64.of_int

let rec read_byte n acc bits =
  bitmatch bits with
    | { true  : 1
      ; b0    : 7
      ; rest  : -1 : bitstring
      } ->
      read_byte (n + 1) (acc +^ (i64 b0 <<< (7 * n))) rest
    | { false : 1
      ; b0    : 7
      ; rest  : -1 : bitstring
      } ->
      Ok (acc +^ (i64 b0 <<< (7 * n)), rest)
    | { _ } ->
      Error `Incomplete

let read = read_byte 0 (i64 0)
