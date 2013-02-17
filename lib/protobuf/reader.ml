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
	Protocol.read_next bits >>= fun (field, bits) ->
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

let run t s = t.run s

let read tag f s =
  let module S = State in
  let open Result.Monad_infix in
  match Int.Map.find s.S.tags tag with
    | Some values ->
      let s = { s with S.tags = Int.Map.remove s.S.tags tag } in
      f values >>= fun a ->
      Ok (a, s)
    | None ->
      f [] >>= fun a ->
      Ok (a, s)

let make_t tag f =
  { run = read tag f }

let bool tag =
  let open Protocol.Value in
  let open Int64 in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Varint v)::_ when v = zero -> Ok true
      | (Varint _)::_               -> Ok false
      | _                           -> Error `Wrong_type)

let int32 tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Varint v)::_ -> begin
	match Int32.of_int64 v with
	  | Some v -> Ok v
	  | None -> Error `Overflow
      end
      | (Fixed32 v)::_ ->
	Ok v
      | _ ->
	Error `Wrong_type)

let int64 tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Varint v)::_ ->
	Ok v
      | (Fixed64 v)::_ ->
	Ok v
      | _ ->
	Error `Wrong_type)

let float tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Fixed32 v)::_ ->
	Ok (Int32.float_of_bits v)
      | _ ->
	Error `Wrong_type)

let double tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Fixed64 v)::_ ->
	Ok (Int64.float_of_bits v)
      | _ ->
	Error `Wrong_type)

let string tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Sequence bits)::_ ->
	Ok (Bitstring.string_of_bitstring bits)
      | _ ->
	Error `Wrong_type)

let embd_msg tag r =
  let open Protocol.Value in
  let open Result.Monad_infix in
  make_t
    tag
    (function
      | [] ->
	Error `Incomplete
      | (Sequence bits)::_ ->
	State.create bits >>= fun s ->
	run r s           >>= fun (a, _) ->
	Ok a
      | _ ->
	Error `Wrong_type)

type 'a _t = 'a t

include (Monad.Make (struct
  type 'a t = 'a _t

  let return a = { run = fun s -> Ok (a, s) }
  let fail err = { run = fun _ -> Error err }

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
end) : Monad.S with type 'a t := 'a _t)
