open Core.Std

type t = Int64.t

type error = [ `Incomplete | `Overflow ]

val read : Bitstring.bitstring -> ((t * Bitstring.bitstring), [> error ]) Result.t
