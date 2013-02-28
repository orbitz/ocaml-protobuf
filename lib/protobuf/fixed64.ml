open Core.Std

type t = Int64.t

type error = [ `Incomplete ]

let next bits =
  bitmatch bits with
    | { n    : 64 : littleendian
      ; rest : -1 : bitstring
      } ->
      Ok (n, rest)
    | { _ } ->
      Error `Incomplete

let to_string t = failwith "nyi"
