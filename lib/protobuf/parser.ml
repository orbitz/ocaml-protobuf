open Core.Std

type error = [ `Incomplete | `Overflow | `Unknown_type | `Wrong_type ]

module State = struct
  type t = { tags : Protocol.Value.t list Int.Map.t
	   }

  let tags_of_bits bits =
    let append field map =
      let module F = Protocol.Field in
      match Int.Map.find map (F.tag field) with
	| Some v ->
	  Int.Map.add ~key:(F.tag field) ~data:((F.value field)::v) map
	| None ->
	  Int.Map.add ~key:(F.tag field) ~data:([F.value field]) map
    in
    let rec tags_of_bits' acc bits =
      if Bitstring.bitstring_length bits > 0 then begin
	let open Result.Monad_infix in
	Protocol.next bits >>= fun (field, bits) ->
	tags_of_bits' (append field acc) bits
      end
      else
	Ok acc
    in
    tags_of_bits' (Int.Map.empty) bits

  let create bits =
    let open Result.Monad_infix in
    tags_of_bits bits >>= fun tags ->
    Ok { tags }

end

type 'a t = { run : State.t -> (('a * State.t), error) Result.t }

type tag = int

type 'a _t = 'a t

let fail err = { run = fun _ -> Error err }

include (Monad.Make (struct
  type 'a t = 'a _t

  let return a = { run = fun s -> Ok (a, s) }

  let bind t f =
    { run = fun s ->
      match t.run s with
	| Ok (a, s') ->
	  let t' = f a in
	  t'.run s'
	| Error `Incomplete ->
	  let t' = fail `Incomplete in
	  t'.run s
	| Error `Overflow ->
	  let t' = fail `Overflow in
	  t'.run s
	| Error `Unknown_type ->
	  let t' = fail `Unknown_type in
	  t'.run s
	| Error `Wrong_type ->
	  let t' = fail `Wrong_type in
	  t'.run s
    }

  let map = `Define_using_bind

end) : Monad.S with type 'a t := 'a _t)


let rec break_foldl ~f ~init = function
  | [] ->
    Ok init
  | x::xs ->
    let open Result.Monad_infix in
    f init x >>= fun init ->
    break_foldl ~f ~init xs

let check_type f l =
  let open Result.Monad_infix in
  let check acc v =
    f v >>= fun v ->
    Ok (v::acc)
  in
  (*
   * The values are put in the tags map in reverse order
   * that they are seen, this foldl will put it back in the
   * correct order
   *)
  break_foldl ~f:check ~init:[] l >>= fun l ->
  Ok l

let rec consume_type d f bits =
  if Bitstring.bitstring_length bits = 0 then
    Ok []
  else begin
    let open Result.Monad_infix in
    d bits                >>= fun (v, rest) ->
    f v                   >>= fun v ->
    consume_type d f rest >>= fun r ->
    Ok (v::r)
  end

let check_pkd_type d f l =
  match List.last l with
  | None ->
    Ok []
  | Some (Protocol.Value.Sequence last) ->
    consume_type d f last
  | Some _ ->
    Error `Wrong_type

let extract_opt l =
  return (List.last l)

let required = function
  | Some x ->
    return x
  | None ->
    fail `Incomplete

(*
 * Handling all the errors is ugly but it's so we
 * can get a n open polymorphic vairant in the type
 * of [run], to make it work nicely in a monad
 *)
let run t s =
  match t.run s with
    | Ok (v, s) ->
      Ok (v, s)
    | Error `Incomplete ->
      Error `Incomplete
    | Error `Overflow ->
      Error `Overflow
    | Error `Unknown_type ->
      Error `Unknown_type
    | Error `Wrong_type ->
      Error `Wrong_type


let read tag f s =
  let module S = State in
  let open Result.Monad_infix in
  match Int.Map.find s.S.tags tag with
    | Some values ->
      let s = { S.tags = Int.Map.remove s.S.tags tag } in
      f values >>= fun a ->
      Ok (a, s)
    | None ->
      f [] >>= fun a ->
      Ok (a, s)

let make_t tag f =
  { run = read tag f }

let enum_rep tag c =
  let open Protocol.Value in
  make_t
    tag
    (check_type
       (function
	 | Varint v -> begin
	   match Int64.to_int v with
	     | Some v -> c v
	     | None   -> Error `Overflow
	 end
	 | _        -> Error `Wrong_type))

let enum_opt tag c =
  enum_rep tag c >>= extract_opt

let enum tag c =
  enum_opt tag c >>= required

let enum_pkd tag c =
  make_t
    tag
    (check_pkd_type
       Varint.of_bitstring
       (fun v ->
	 match Int64.to_int v with
	   | Some v -> c v
	   | None   -> Error `Overflow))

let bool_conv = function
  | 0 -> Ok false
  | 1 -> Ok true
  | _ -> Error `Overflow

let bool_rep tag =
  let open Protocol.Value in
  let open Int64 in
  enum_rep
    tag
    bool_conv

let bool_opt tag =
  bool_rep tag >>= extract_opt

let bool tag =
  bool_opt tag >>= required

let bool_pkd tag =
  let open Protocol.Value in
  let open Int64 in
  enum_pkd
    tag
    bool_conv

let int32_rep tag =
  let open Protocol.Value in
  make_t
    tag
    (check_type
       (function
	 | Varint v -> begin
	   match Int32.of_int64 v with
	     | Some v -> Ok v
	     | None   -> Error `Overflow
	 end
	 | Fixed32 v ->
	   Ok v
	 | _ ->
	   Error `Wrong_type))

let int32_opt tag =
  int32_rep tag >>= extract_opt

let int32 tag =
  int32_opt tag >>= required

let int32_pkd tag =
  make_t
    tag
    (check_pkd_type
       Varint.of_bitstring
       (fun v ->
	 match Int32.of_int64 v with
	   | Some v -> Ok v
	   | None   -> Error `Overflow))

let sint32_rep tag =
  int32_rep tag >>= fun ints ->
  return (List.map ~f:Sint32.decode ints)

let sint32_opt tag =
  sint32_rep tag >>= extract_opt

let sint32 tag =
  sint32_opt tag >>= required

let sint32_pkd tag =
  int32_pkd tag >>= fun ints ->
  return (List.map ~f:Sint32.decode ints)

let int64_rep tag =
  let open Protocol.Value in
  make_t
    tag
    (check_type
       (function
	 | Varint v ->
	   Ok v
	 | Fixed64 v ->
	   Ok v
	 | _ ->
	   Error `Wrong_type))

let int64_opt tag =
  int64_rep tag >>= extract_opt

let int64 tag =
  int64_opt tag >>= required

let int64_pkd tag =
  make_t
    tag
    (check_pkd_type
       Varint.of_bitstring
       (fun v -> Ok v))

let sint64_rep tag =
  int64_rep tag >>= fun ints ->
  return (List.map ~f:Sint64.decode ints)

let sint64_opt tag =
  sint64_rep tag >>= extract_opt

let sint64 tag =
  sint64_opt tag >>= required

let sint64_pkd tag =
  int64_pkd tag >>= fun ints ->
  return (List.map ~f:Sint64.decode ints)

let float_rep tag =
  let open Protocol.Value in
  make_t
    tag
    (check_type
       (function
	 | Fixed32 v ->
	   Ok (Int32.float_of_bits v)
	 | _ ->
	   Error `Wrong_type))

let float_opt tag =
  float_rep tag >>= extract_opt

let float tag =
  float_opt tag >>= required

let float_pkd tag =
  make_t
    tag
    (check_pkd_type
       Fixed32.of_bitstring
       (fun v -> Ok (Int32.float_of_bits v)))

let double_rep tag =
  let open Protocol.Value in
  make_t
    tag
    (check_type
       (function
	 | Fixed64 v ->
	   Ok (Int64.float_of_bits v)
	 | _ ->
	   Error `Wrong_type))

let double_opt tag =
  double_rep tag >>= extract_opt

let double tag =
  double_opt tag >>= required

let double_pkd tag =
  make_t
    tag
    (check_pkd_type
       Fixed64.of_bitstring
       (fun v -> Ok (Int64.float_of_bits v)))

let bytes_rep tag =
  let open Protocol.Value in
  make_t
    tag
    (check_type
       (function
	 | Sequence bits ->
	   Ok (Bitstring.string_of_bitstring bits)
	 | _ ->
	   Error `Wrong_type))

let bytes_opt tag =
  bytes_rep tag >>= extract_opt

let bytes tag =
  bytes_opt tag >>= required

let string_rep = bytes_rep
let string_opt = bytes_opt
let string     = bytes

let embd_msg_rep tag r =
  let open Protocol.Value in
  let open Result.Monad_infix in
  make_t
    tag
    (check_type
       (function
	 | Sequence bits ->
	   State.create bits >>= fun s ->
	   run r s           >>= fun (a, _) ->
	   Ok a
	 | _ ->
	   Error `Wrong_type))

let embd_msg_opt tag r =
  embd_msg_rep tag r >>= extract_opt

let embd_msg tag r =
  embd_msg_opt tag r >>= required
