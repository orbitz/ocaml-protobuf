open Core.Std

type t = Int64.t

type error = [ `Incomplete | `Overflow ]

let s7 i = Int64.shift_left i 7
let (+)  = Int64.(+)
let i64  = Int64.of_int

let rec read_byte acc bits =
  bitmatch bits with
    | { true  : 1
      ; b0    : 7
      ; rest  : -1 : bitstring
      } ->
      read_byte (s7 (acc + i64 b0)) rest
    | { false : 1
      ; b0    : 7
      ; rest  : -1 : bitstring
      } ->
      Ok (acc + i64 b0, rest)
    | { _ } ->
      Error `Incomplete

let read = read_byte (i64 0)
