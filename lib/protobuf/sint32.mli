open Core.Std

type t = Int32.t

val encode : Int32.t -> t
val decode : t -> Int32.t
