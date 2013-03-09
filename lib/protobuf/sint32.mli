open Core.Std

type t = Int32.t

type error = [ `Incomplete | `Overflow ]

val of_bitstring : Bitstring.bitstring -> ((t * Bitstring.bitstring), [> error ]) Result.t
val to_string    : t -> string
