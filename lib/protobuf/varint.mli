open Core.Std

type t = Int64.t

type error = [ `Incomplete | `Overflow ]

val of_bitstring : Bitstring.bitstring -> ((t * Bitstring.bitstring), [> error ]) Result.t
val to_bitstring : t -> Bitstring.bitstring
val of_string    : string -> ((t * Bitstring.bitstring), [> error ]) Result.t
val to_string    : t -> string
