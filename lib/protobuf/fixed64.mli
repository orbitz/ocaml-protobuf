open Core.Std

type t = Int64.t

type error = [ `Incomplete ]

val of_bitstring : Bitstring.bitstring -> ((t * Bitstring.bitstring), [> error ]) Result.t
val to_string    : t -> string
