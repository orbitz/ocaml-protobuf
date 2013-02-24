open Core.Std

module Value = Protocol.Value

type t = Buffer.t

type error = [ `Overflow ]

type tag = int

let create () = Buffer.create 100

let to_string = Buffer.contents

let to_field tag v =
  Protocol.Field.create tag v

let varint t tag v =
  let f = to_field tag (Value.Varint v) in
  Ok (Buffer.add_string t (Protocol.to_string f))

let enum t tag v f =
  failwith "nyi"

let bool t tag v =
  failwith "nyi"

let int64 t tag v =
  failwith "nyi"

let int32 t tag v =
  failwith "nyi"

let fixed64 t tag v =
  failwith "nyi"

let fixed32 t tag v =
  failwith "nyi"

let sint64 t tag v =
  failwith "nyi"

let sint32 t tag v =
  failwith "nyi"

let double t tag v =
  failwith "nyi"

let float t tag v =
  failwith "nyi"

let string t tag v =
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
  Ok (Buffer.add_string t (Protocol.to_string f))

let builder t tag v =
  failwith "nyi"

let embd_msg t tag v f =
  failwith "nyi"
