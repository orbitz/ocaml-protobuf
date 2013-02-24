open Core.Std
open Protobuf.Parser.Monad_infix
module P = Protobuf.Parser
module B = Protobuf.Builder

let simple_bits = "\x08\xAC\x02"
let simple =
  P.int32 1 >>= P.return

let string_bits = "\x12\x07\x74\x65\x73\x74\x69\x6e\x67"
let string =
  P.string 2 >>= P.return

let embd_bits = "\x1a\x03\x08\x96\x01"
let embd =
  P.embd_msg 3 simple >>= P.return

let complex_bits = "\x08\xAC\x02\x12\x07\x74\x65\x73\x74\x69\x6e\x67\x1a\x03\x08\x96\x01"
let complex =
  P.int32 1           >>= fun num ->
  P.string 2          >>= fun s ->
  P.embd_msg 3 simple >>= fun emsg ->
  P.return (num, s, emsg)

let incomplete_bits = "\x08\xAC"
let incomplete = simple

let wrong_type_bits = simple_bits
let wrong_type =
  P.string 1 >>= P.return

let unordered_bits = complex_bits
let unordered =
  P.string 2          >>= fun s ->
  P.embd_msg 3 simple >>= fun emsg ->
  P.int32 1           >>= fun num ->
  P.return (num, s, emsg)

let rep_1_bits = simple_bits
let rep_1 =
  P.int32_rep 1 >>= P.return

let rep_2_bits = simple_bits ^ simple_bits
let rep_2 =
  P.int32_rep 1 >>= P.return

let rep_3_bits = simple_bits ^ "\x08\x96\x01"
let rep_3 =
  P.int32_rep 1 >>= P.return

let opt_1_bits = ""
let opt_1 =
  P.int32_opt 1 >>= P.return

let dups_1_bits = rep_3_bits
let dups_1 =
  P.int32 1 >>= P.return

let enum_bits = simple_bits
type enum_t = Foo
let enum_conv = function
  | 300 -> Ok Foo
  | _ -> Error `Overflow
let enum =
  P.enum 1 enum_conv >>= P.return

let build_simple () =
  let open Result.Monad_infix in
  let b = B.create () in
  B.varint b 1 (Int64.of_int 300) >>= fun () ->
  Ok (B.to_string b)

let build_string () =
  let open Result.Monad_infix in
  let b = B.create () in
  B.string b 2 "testing" >>= fun () ->
  Ok (B.to_string b)

let assert_p_success v = function
  | Ok (r, _) ->
    assert (r = v)
  | _ ->
    assert false

let assert_p_error err = function
  | Error e when e = err ->
    ()
  | _ ->
    assert false

let assert_b_success v = function
  | Ok r ->
    assert (r = v)
  | _ ->
    assert false

let run r s =
  let open Result.Monad_infix in
  P.State.create (Bitstring.bitstring_of_string s) >>= fun s ->
  P.run r s

let main () =
  assert_p_success (Int32.of_int_exn 300) (run simple simple_bits);
  assert_p_success "testing" (run string string_bits);
  assert_p_success (Int32.of_int_exn 150) (run embd embd_bits);
  assert_p_success
    (Int32.of_int_exn 300, "testing", Int32.of_int_exn 150)
    (run complex complex_bits);
  assert_p_error `Incomplete (run incomplete incomplete_bits);
  assert_p_error `Wrong_type (run wrong_type wrong_type_bits);
  assert_p_success
    (Int32.of_int_exn 300, "testing", Int32.of_int_exn 150)
    (run unordered unordered_bits);
  assert_p_success
    [Int32.of_int_exn 300]
    (run rep_1 rep_1_bits);
  assert_p_success
    [Int32.of_int_exn 300; Int32.of_int_exn 300]
    (run rep_2 rep_2_bits);
  assert_p_success
    None
    (run opt_1 opt_1_bits);
  assert_p_success
    [Int32.of_int_exn 300; Int32.of_int_exn 150]
    (run rep_3 rep_3_bits);
  assert_p_success
    (Int32.of_int_exn 150)
    (run dups_1 dups_1_bits);
  assert_p_success
    Foo
    (run enum enum_bits);
  assert_b_success
    simple_bits
    (build_simple ());
  assert_b_success
    string_bits
    (build_string ())

let () = main ()
