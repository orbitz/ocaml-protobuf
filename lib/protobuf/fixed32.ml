module Old_int32 = Int32

open Core.Std

type t = Int32.t

type error = [ `Incomplete ]

let of_bitstring bits =
  let module Int32 = Old_int32 in
  bitmatch bits with
    | { n    : 32 : littleendian
      ; rest : -1 : bitstring
      } ->
      Ok (n, rest)
    | { _ } ->
      Error `Incomplete

let shift_and_char t n =
  let open Int32 in
  Char.of_int_exn (to_int_exn (shift_right t n) land 0xff)

let to_string t =
  let s = String.create 4 in
  s.[3] <- shift_and_char t 24;
  s.[2] <- shift_and_char t 16;
  s.[1] <- shift_and_char t 8;
  s.[0] <- Char.of_int_exn ((Int32.to_int_exn t) land 0xff);
  s

