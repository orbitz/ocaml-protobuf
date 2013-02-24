open Core.Std

type t

type error = [ `Overflow ]

type tag = int

val create    : unit -> t
val to_string : t -> string

val varint   : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val enum     : t -> tag -> 'a -> ('a -> (int, [> error ]) Result.t) -> (unit, [> error ]) Result.t
val bool     : t -> tag -> bool -> (unit, [> error ]) Result.t
val int64    : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val int32    : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val fixed64  : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val fixed32  : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val sint64   : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val sint32   : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val double   : t -> tag -> Float.t -> (unit, [> error ]) Result.t
val float    : t -> tag -> Float.t -> (unit, [> error ]) Result.t
val string   : t -> tag -> string -> (unit, [> error ]) Result.t
val builder  : t -> tag -> t -> (unit, [> error ]) Result.t
val embd_msg : t -> tag -> 'a -> ('a -> (t, [> error ]) Result.t) -> (unit, [> error ]) Result.t
