(* Need to save this for Bitstring *)
module Old_int32 = Int32

open Core.Std

module Value = struct
  type t =
    | Varint   of Int64.t
    | Fixed64  of Int64.t
    | Fixed32  of Int32.t
    | Sequence of Bitstring.bitstring
end

module Field = struct
  type t = { tag   : int
	   ; value : Value.t
	   }


  let tag   t = t.tag
  let value t = t.value
end

type error = [ `Incomplete | `Overflow | `Unknown_type ]

let extract_tag field =
  let tag = Int64.shift_right field 3 in
  match Int64.to_int tag with
    | Some x -> x
    | None   ->
      failwith
	(Printf.sprintf "Tag too large: %s" (Int64.to_string tag))

let v_type_mask = Int64.of_int 7
let extract_v_type field =
  Int64.to_int_exn (Int64.bit_and field v_type_mask)

let int_of_int64 n =
  match Int64.to_int n with
    | Some n ->
      Ok n
    | None ->
      Error `Incomplete

let read_varint bits =
  let open Result.Monad_infix in
  Varint.read bits >>= fun (n, rest) ->
  Ok (Value.Varint n, rest)

let read_fixed64 bits =
  bitmatch bits with
    | { n : 64 : littleendian
      ; rest : -1 : bitstring
      } ->
      Ok (Value.Fixed64 n, rest)
    | { _ } ->
      Error `Incomplete

let read_sequence bits =
  let open Result.Monad_infix in
  Varint.read bits    >>= fun (length, rest) ->
  int_of_int64 length >>= fun length ->
  bitmatch rest with
    | { seq : length * 8 : bitstring
      ; rest : -1 : bitstring
      } ->
      Ok (Value.Sequence seq, rest)
    | { _ } ->
      Error `Incomplete

let read_fixed32 bits =
  let module Int32 = Old_int32 in
  bitmatch bits with
    | { n : 32 : littleendian
      ; rest : -1 : bitstring
      } ->
      Ok (Value.Fixed32 n, rest)
    | { _ } ->
      Error `Incomplete

let read_v_type v_type rest =
  match v_type with
    | 0 -> read_varint rest
    | 1 -> read_fixed64 rest
    | 2 -> read_sequence rest
    | 5 -> read_fixed32 rest
    | _ -> Error `Unknown_type

let read_next bits =
  let open Result.Monad_infix in
  Varint.read bits >>= fun (field, rest) ->
  let tag = extract_tag field in
  let v_type = extract_v_type field in
  read_v_type v_type rest >>= fun (value, rest) ->
  Ok ({Field.tag = tag; value}, rest)

