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

let when_some f = function
  | Some v -> f v
  | None   -> Ok ()

let enum t tag v conv =
  let open Result.Monad_infix in
  conv v >>= fun v ->
  let f = to_field tag (Value.Varint (Int64.of_int v)) in
  add_field t f

let enum_opt t tag v_opt conv =
  when_some
    (fun v -> enum t tag v conv)
    v_opt

let rec enum_rep t tag vs conv =
  match vs with
    | [] -> Ok ()
    | x::xs -> begin
      let open Result.Monad_infix in
      enum t tag x conv >>= fun () ->
      enum_rep t tag xs conv
    end

let bool t tag v =
  let f =
    if v then
      to_field tag (Value.Varint Int64.one)
    else
      to_field tag (Value.Varint Int64.zero)
  in
  add_field t f

let bool_opt t tag v_opt =
  when_some
    (bool t tag)
    v_opt

let rec bool_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    bool t tag x >>= fun () ->
    bool_rep t tag xs
  end

let int64 t tag v =
  let f = to_field tag (Value.Varint v) in
  add_field t f

let int64_opt t tag v_opt =
  when_some
    (int64 t tag)
    v_opt

let rec int64_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    int64 t tag x >>= fun () ->
    int64_rep t tag xs
  end

let int32 t tag v =
  let f = to_field tag (Value.Varint (Int64.of_int32 v)) in
  add_field t f

let int32_opt t tag v_opt =
  when_some
    (int32 t tag)
    v_opt

let rec int32_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    int32 t tag x >>= fun () ->
    int32_rep t tag xs
  end

let fixed64 t tag v =
  let f = to_field tag (Value.Fixed64 v) in
  add_field t f

let fixed64_opt t tag v_opt =
  when_some
    (fixed64 t tag)
    v_opt

let rec fixed64_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    fixed64 t tag x >>= fun () ->
    fixed64_rep t tag xs
  end

let fixed32 t tag v =
  let f = to_field tag (Value.Fixed32 v) in
  add_field t f

let fixed32_opt t tag v_opt =
  when_some
    (fixed32 t tag)
    v_opt

let rec fixed32_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    fixed32 t tag x >>= fun () ->
    fixed32_rep t tag xs
  end

let sint64 t tag v =
  let f = to_field tag (Value.Varint (Sint64.encode v)) in
  add_field t f

let sint64_opt t tag v_opt =
  when_some
    (sint64 t tag)
    v_opt

let rec sint64_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    sint64 t tag x >>= fun () ->
    sint64_rep t tag xs
  end

let sint32 t tag v =
  let f = to_field tag (Value.Varint
			  (Int64.of_int32
			     (Sint32.encode v)))
  in
  add_field t f

let sint32_opt t tag v_opt =
  when_some
    (sint32 t tag)
    v_opt

let rec sint32_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    sint32 t tag x >>= fun () ->
    sint32_rep t tag xs
  end

let double t tag v =
  let f = to_field tag (Value.Fixed64 (Int64.bits_of_float v)) in
  add_field t f

let double_opt t tag v_opt =
  when_some
    (double t tag)
    v_opt

let rec double_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    double t tag x >>= fun () ->
    double_rep t tag xs
  end

let float t tag v =
  let open Result.Monad_infix in
  let i64 = Int64.bits_of_float v in
  int32_of_int64 i64 >>= fun v ->
  let f = to_field tag (Value.Fixed32 v) in
  add_field t f

let float_opt t tag v_opt =
  when_some
    (float t tag)
    v_opt

let rec float_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    float t tag x >>= fun () ->
    float_rep t tag xs
  end


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

let bytes_opt t tag v_opt =
  when_some
    (bytes t tag)
    v_opt

let rec bytes_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    bytes t tag x >>= fun () ->
    bytes_rep t tag xs
  end

let string = bytes

let string_opt t tag v_opt =
  when_some
    (string t tag)
    v_opt

let rec string_rep t tag = function
  | [] -> Ok ()
  | x::xs -> begin
    let open Result.Monad_infix in
    string t tag x >>= fun () ->
    string_rep t tag xs
  end

let embd_msg t tag v conv =
  let open Result.Monad_infix in
  conv v >>= fun v ->
  string t tag v

let embd_msg_opt t tag v_opt conv =
  when_some
    (fun v -> embd_msg t tag v conv)
    v_opt

let rec embd_msg_rep t tag vs conv =
  match vs with
    | [] -> Ok ()
    | x::xs -> begin
      let open Result.Monad_infix in
      embd_msg t tag x conv >>= fun () ->
      embd_msg_rep t tag xs conv
    end
