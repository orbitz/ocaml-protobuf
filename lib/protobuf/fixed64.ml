open Core.Std

type t = Int64.t

type error = [ `Incomplete ]

let of_bitstring bits =
  bitmatch bits with
    | { n    : 64 : littleendian
      ; rest : -1 : bitstring
      } ->
      Ok (n, rest)
    | { _ } ->
      Error `Incomplete

let shift_and_char t n =
  let open Int64 in
  Char.of_int_exn (to_int_exn (shift_right t n) land 0xff)

let to_string t =
  let s = String.create 8 in
  s.[7] <- shift_and_char t 56;
  s.[6] <- shift_and_char t 48;
  s.[5] <- shift_and_char t 40;
  s.[4] <- shift_and_char t 32;
  s.[4] <- shift_and_char t 24;
  s.[2] <- shift_and_char t 16;
  s.[1] <- shift_and_char t 8;
  s.[0] <- Char.of_int_exn ((Int64.to_int_exn t) land 0xff);
  s
