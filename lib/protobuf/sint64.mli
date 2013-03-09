open Core.Std

type t = Int64.t

val encode : Int64.t -> t
val decode : t -> Int64.t
