open Core.Std

type error = [ `Incomplete | `Overflow | `Unknown_type | `Wrong_type ]

module State = struct
  type t = { bits    : Bitstring.bitstring
	   ; skipped : Protocol.Value.t Int32.Map.t
	   }

  let create bits = { bits; skipped = Int32.Map.empty }
  let append t b = { t with bits = Bitstring.concat [t.bits; b] }
end

type 'a t = { run : State.t -> (('a * State.t), error) Result.t }

type tag = int

let run t s = t.run s

let rec read tag f s =
  let module P = Protocol in
  let module F = P.Field in
  let open Result.Monad_infix in
  P.read_next s.State.bits >>= function
    | (field, bits) when F.tag field = tag -> begin
      f (F.value field) >>= fun a ->
      Ok (a, {s with State.bits = bits })
    end
    | _ ->
      failwith "nyi"

let make_t tag f =
  { run = read tag f }

let bool tag =
  let open Protocol.Value in
  let open Int64 in
  make_t
    tag
    (function
      | Varint v when v = zero -> Ok true
      | Varint _ -> Ok false
      | _        -> Error `Wrong_type)

let int32 tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | Varint v -> begin
	match Int32.of_int64 v with
	  | Some v -> Ok v
	  | None -> Error `Overflow
      end
      | Fixed32 v ->
	Ok v
      | _ ->
	Error `Wrong_type)

let int64 tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | Varint v ->
	Ok v
      | Fixed64 v ->
	Ok v
      | _ ->
	Error `Wrong_type)

let float tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | Fixed32 v ->
	Ok (Int32.float_of_bits v)
      | _ ->
	Error `Wrong_type)

let double tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | Fixed64 v ->
	Ok (Int64.float_of_bits v)
      | _ ->
	Error `Wrong_type)

let string tag =
  let open Protocol.Value in
  make_t
    tag
    (function
      | Sequence bits ->
	Ok (Bitstring.string_of_bitstring bits)
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
