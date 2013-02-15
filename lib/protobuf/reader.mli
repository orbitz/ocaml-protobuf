open Core.Std

type error = [ `Incomplete | `Overflow | `Unknown_type | `Wrong_type ]

type 'a t

module State : sig
  type t

  val create : Bitstring.bitstring -> t
  val append : t -> Bitstring.bitstring -> t
end

include Monad.S with type 'a t := 'a t

type tag = int

val run    : 'a t -> State.t -> (('a * State.t), error) Result.t
val bind   : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t

val bool   : tag -> bool t
val int32  : tag -> Int32.t t
val int64  : tag -> Int64.t t
val float  : tag -> Float.t t
val double : tag -> Float.t t
val string : tag -> String.t t
