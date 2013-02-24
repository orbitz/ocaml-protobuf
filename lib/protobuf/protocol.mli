open Core.Std

module Value : sig
  type t =
    | Varint   of Int64.t
    | Fixed64  of Int64.t
    | Fixed32  of Int32.t
    | Sequence of Bitstring.bitstring
end

module Field : sig
  type t

  val create : int -> Value.t -> t
  val tag    : t -> int
  val value  : t -> Value.t
end

type error = [ `Incomplete | `Overflow | `Unknown_type ]

val next : Bitstring.bitstring ->
           ((Field.t * Bitstring.bitstring), [> error ]) Result.t

val to_string : Field.t -> string

