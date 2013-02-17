open Core.Std
open Protobuf.Reader.Monad_infix
module R = Protobuf.Reader

let simple_bits = "\x08\xAC\x02"
let simple =
  R.int32 1 >>= R.return

let string_bits = "\x12\x07\x74\x65\x73\x74\x69\x6e\x67"
let string =
  R.string 2 >>= R.return

let embd_bits = "\x1a\x03\x08\x96\x01"
let embd =
  R.embd_msg 3 simple >>= R.return

let complex_bits = "\x08\xAC\x02\x12\x07\x74\x65\x73\x74\x69\x6e\x67\x1a\x03\x08\x96\x01"
let complex =
  R.int32 1           >>= fun num ->
  R.string 2          >>= fun s ->
  R.embd_msg 3 simple >>= fun emsg ->
  R.return (num, s, emsg)

let incomplete_bits = "\x08\xAC"
let incomplete = simple

let wrong_type_bits = simple_bits
let wrong_type =
  R.string 1 >>= R.return

let unordered_bits = complex_bits
let unordered =
  R.string 2          >>= fun s ->
  R.embd_msg 3 simple >>= fun emsg ->
  R.int32 1           >>= fun num ->
  R.return (num, s, emsg)

let rep_1_bits = simple_bits
let rep_1 =
  R.int32_rep 1 >>= R.return

let rep_2_bits = simple_bits ^ simple_bits
let rep_2 =
  R.int32_rep 1 >>= R.return

let opt_1_bits = ""
let opt_1 =
  R.int32_opt 1 >>= R.return

let assert_success v = function
  | Ok (r, _) ->
    assert (r = v)
  | _ ->
    assert false

let assert_error err = function
  | Error e when e = err ->
    ()
  | _ ->
    assert false

let run r s =
  let open Result.Monad_infix in
  R.State.create (Bitstring.bitstring_of_string s) >>= fun s ->
  R.run r s

let main () =
  assert_success (Int32.of_int_exn 300) (run simple simple_bits);
  assert_success "testing" (run string string_bits);
  assert_success (Int32.of_int_exn 150) (run embd embd_bits);
  assert_success
    (Int32.of_int_exn 300, "testing", Int32.of_int_exn 150)
    (run complex complex_bits);
  assert_error `Incomplete (run incomplete incomplete_bits);
  assert_error `Wrong_type (run wrong_type wrong_type_bits);
  assert_success
    (Int32.of_int_exn 300, "testing", Int32.of_int_exn 150)
    (run unordered unordered_bits);
  assert_success
    [Int32.of_int_exn 300]
    (run rep_1 rep_1_bits);
  assert_success
    [Int32.of_int_exn 300; Int32.of_int_exn 300]
    (run rep_2 rep_2_bits);
  assert_success
    None
    (run opt_1 opt_1_bits)

let () = main ()
