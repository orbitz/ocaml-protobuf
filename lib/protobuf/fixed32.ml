module Old_int32 = Int32

open Core.Std

type t = Int32.t

type error = [ `Incomplete ]

let next bits =
  let module Int32 = Old_int32 in
  bitmatch bits with
    | { n    : 32 : littleendian
      ; rest : -1 : bitstring
      } ->
      Ok (n, rest)
    | { _ } ->
      Error `Incomplete

let to_string t = failwith "nyi"
