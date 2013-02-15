open Core.Std
open Protobuf.Reader.Monad_infix
module R = Protobuf.Reader

let simple_s = R.State.create (Bitstring.bitstring_of_string "\x08\xAC\x02")
let simple =
  R.int32 1 >>= R.return

let string_s =
  R.State.create (Bitstring.bitstring_of_string "\x12\x07\x74\x65\x73\x74\x69\x6e\x67")
let string =
  R.string 2 >>= R.return

let embd_s = R.State.create (Bitstring.bitstring_of_string "\x1a\x03\x08\x96\x01")
let embd =
  R.embd_msg 3 simple >>= R.return

let complex_s =
  R.State.create (Bitstring.bitstring_of_string
		    "\x08\xAC\x02\x12\x07\x74\x65\x73\x74\x69\x6e\x67\x1a\x03\x08\x96\x01")
let complex =
  R.int32 1           >>= fun num ->
  R.string 2          >>= fun s ->
  R.embd_msg 3 simple >>= fun emsg ->
  R.return (num, s, emsg)

let incomplete_s = R.State.create (Bitstring.bitstring_of_string "\x08\xAC")
let incomplete = simple

let wrong_type_s = simple_s
let wrong_type =
  R.string 1 >>= R.return

let unordered_s = complex_s
let unordered =
  R.string 2          >>= fun s ->
  R.embd_msg 3 simple >>= fun emsg ->
  R.int32 1           >>= fun num ->
  R.return (num, s, emsg)

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

let main () =
  assert_success (Int32.of_int_exn 300) (R.run simple simple_s);
  assert_success "testing" (R.run string string_s);
  assert_success (Int32.of_int_exn 150) (R.run embd embd_s);
  assert_success
    (Int32.of_int_exn 300, "testing", Int32.of_int_exn 150)
    (R.run complex complex_s);
  assert_error `Incomplete (R.run incomplete incomplete_s);
  assert_error `Wrong_type (R.run wrong_type wrong_type_s);
  assert_success
    (Int32.of_int_exn 300, "testing", Int32.of_int_exn 150)
    (R.run unordered unordered_s)

let () = main ()
