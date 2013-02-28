open Core.Std

type t = Int32.t

type error = [ `Incomplete ]

val next      : Bitstring.bitstring -> ((t * Bitstring.bitstring), [> error ]) Result.t
val to_string : t -> string
