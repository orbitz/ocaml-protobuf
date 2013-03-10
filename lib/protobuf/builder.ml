open Core.Std

module Value = Protocol.Value

type t = Buffer.t

type error = [ `Overflow ]

type tag = int

let create () = Buffer.create 100

let to_string = Buffer.contents

let to_field tag v =
  Protocol.Field.create tag v

let int32_of_int64 v =
  match Int32.of_int64 v with
    | Some v ->
      Ok v
    | None ->
      Error `Overflow

let add_field t f =
  Buffer.add_string t (Protocol.to_string f);
  Ok ()

let enum t tag v conv =
  let open Result.Monad_infix in
  conv v >>= fun v ->
  let f = to_field tag (Value.Varint (Int64.of_int v)) in
  add_field t f

let bool t tag v =
  let f =
    if v then
      to_field tag (Value.Varint Int64.one)
    else
      to_field tag (Value.Varint Int64.zero)
  in
  add_field t f

let int64 t tag v =
  let f = to_field tag (Value.Varint v) in
  add_field t f

let int32 t tag v =
  let f = to_field tag (Value.Varint (Int64.of_int32 v)) in
  add_field t f

let fixed64 t tag v =
  let f = to_field tag (Value.Fixed64 v) in
  add_field t f

let fixed32 t tag v =
  let f = to_field tag (Value.Fixed32 v) in
  add_field t f

let sint64 t tag v =
  let f = to_field tag (Value.Varint (Sint64.encode v)) in
  add_field t f

let sint32 t tag v =
  let f = to_field tag (Value.Varint
			  (Int64.of_int32
			     (Sint32.encode v)))
  in
  add_field t f

let double t tag v =
  let f = to_field tag (Value.Fixed64 (Int64.bits_of_float v)) in
  add_field t f

let float t tag v =
  let open Result.Monad_infix in
  let i64 = Int64.bits_of_float v in
  int32_of_int64 i64 >>= fun v ->
  let f = to_field tag (Value.Fixed32 v) in
  add_field t f

let bytes t tag v =
  (*
   * NOTE:
   * Going back and forth between a string
   * and a bitstring is not free.  Might
   * need to adjust protocol to not require
   * the conversion
   *)
  let f =
    to_field
      tag
      (Value.Sequence (Bitstring.bitstring_of_string v))
  in
  add_field t f

let string = bytes

let embd_msg t tag v conv =
  let open Result.Monad_infix in
  conv v >>= fun v ->
  string t tag v
