open Core.Std

type error = [ `Incomplete | `Overflow | `Unknown_type | `Wrong_type ]

type 'a t

module State : sig
  type t

  val create : Bitstring.bitstring -> (t, [> Protocol.error ]) Result.t
end

include Monad.S with type 'a t := 'a t

type tag = int

val run          : 'a t -> State.t -> (('a * State.t), error) Result.t

val enum         : tag -> (int -> ('a, error) Result.t) -> 'a t
val bool         : tag -> bool t
val int32        : tag -> Int32.t t
val sint32       : tag -> Int32.t t
val int64        : tag -> Int64.t t
val sint64       : tag -> Int64.t t
val float        : tag -> Float.t t
val double       : tag -> Float.t t
val string       : tag -> String.t t
val embd_msg     : tag -> 'a t -> 'a t

val enum_opt     : tag -> (int -> ('a, error) Result.t) -> 'a option t
val bool_opt     : tag -> bool option t
val int32_opt    : tag -> Int32.t option t
val sint32_opt   : tag -> Int32.t option t
val int64_opt    : tag -> Int64.t option t
val sint64_opt   : tag -> Int64.t option t
val float_opt    : tag -> Float.t option t
val double_opt   : tag -> Float.t option t
val string_opt   : tag -> String.t option t
val embd_msg_opt : tag -> 'a t -> 'a option t

val enum_rep     : tag -> (int -> ('a, error) Result.t) -> 'a list t
val bool_rep     : tag -> bool list t
val int32_rep    : tag -> Int32.t list t
val sint32_rep   : tag -> Int32.t list t
val int64_rep    : tag -> Int64.t list t
val sint64_rep   : tag -> Int64.t list t
val float_rep    : tag -> Float.t list t
val double_rep   : tag -> Float.t list t
val string_rep   : tag -> String.t list t
val embd_msg_rep : tag -> 'a t -> 'a list t

val enum_pkd     : tag -> (int -> ('a, error) Result.t) -> 'a list t
val bool_pkd     : tag -> bool list t
val int32_pkd    : tag -> Int32.t list t
val sint32_pkd   : tag -> Int32.t list t
val int64_pkd    : tag -> Int64.t list t
val sint64_pkd   : tag -> Int64.t list t
val float_pkd    : tag -> Float.t list t
val double_pkd   : tag -> Float.t list t
