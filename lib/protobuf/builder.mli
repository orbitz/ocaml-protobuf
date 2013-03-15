open Core.Std

type t

type error = [ `Overflow ]

type tag = int

val create    : unit -> t
val to_string : t -> string

val enum :
  t ->
  tag ->
  'a ->
  ('a -> (int, [> error ] as 'b) Result.t) ->
  (unit, 'b) Result.t

val enum_opt :
  t ->
  tag ->
  'a option ->
  ('a -> (int, [> error ] as 'b) Result.t) ->
  (unit, 'b) Result.t

val enum_rep :
  t ->
  tag ->
  'a list ->
  ('a -> (int, [> error ] as 'b) Result.t) ->
  (unit, 'b) Result.t

val bool        : t -> tag -> bool -> (unit, [> error ]) Result.t
val bool_opt    : t -> tag -> bool option -> (unit, [> error ]) Result.t
val bool_rep    : t -> tag -> bool list -> (unit, [> error ]) Result.t
val int64       : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val int64_opt   : t -> tag -> Int64.t option -> (unit, [> error ]) Result.t
val int64_rep   : t -> tag -> Int64.t list -> (unit, [> error ]) Result.t
val int32       : t -> tag -> Int32.t -> (unit, [> error ]) Result.t
val int32_opt   : t -> tag -> Int32.t option -> (unit, [> error ]) Result.t
val int32_rep   : t -> tag -> Int32.t list -> (unit, [> error ]) Result.t
val fixed64     : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val fixed64_opt : t -> tag -> Int64.t option -> (unit, [> error ]) Result.t
val fixed64_rep : t -> tag -> Int64.t list -> (unit, [> error ]) Result.t
val fixed32     : t -> tag -> Int32.t -> (unit, [> error ]) Result.t
val fixed32_opt : t -> tag -> Int32.t option -> (unit, [> error ]) Result.t
val fixed32_rep : t -> tag -> Int32.t list -> (unit, [> error ]) Result.t
val sint64      : t -> tag -> Int64.t -> (unit, [> error ]) Result.t
val sint64_opt  : t -> tag -> Int64.t option -> (unit, [> error ]) Result.t
val sint64_rep  : t -> tag -> Int64.t list -> (unit, [> error ]) Result.t
val sint32      : t -> tag -> Int32.t -> (unit, [> error ]) Result.t
val sint32_opt  : t -> tag -> Int32.t option -> (unit, [> error ]) Result.t
val sint32_rep  : t -> tag -> Int32.t list -> (unit, [> error ]) Result.t
val double      : t -> tag -> Float.t -> (unit, [> error ]) Result.t
val double_opt  : t -> tag -> Float.t option -> (unit, [> error ]) Result.t
val double_rep  : t -> tag -> Float.t list -> (unit, [> error ]) Result.t
val float       : t -> tag -> Float.t -> (unit, [> error ]) Result.t
val float_opt   : t -> tag -> Float.t option -> (unit, [> error ]) Result.t
val float_rep   : t -> tag -> Float.t list -> (unit, [> error ]) Result.t
val string      : t -> tag -> string -> (unit, [> error ]) Result.t
val string_opt  : t -> tag -> string option -> (unit, [> error ]) Result.t
val string_rep  : t -> tag -> string list -> (unit, [> error ]) Result.t
val bytes       : t -> tag -> string -> (unit, [> error ]) Result.t
val bytes_opt   : t -> tag -> string option-> (unit, [> error ]) Result.t
val bytes_rep   : t -> tag -> string list-> (unit, [> error ]) Result.t

val embd_msg :
  t ->
  tag ->
  'a ->
  ('a -> (string, [> error ] as 'b) Result.t) ->
  (unit, 'b) Result.t

val embd_msg_opt :
  t ->
  tag ->
  'a option ->
  ('a -> (string, [> error ] as 'b) Result.t) ->
  (unit, 'b) Result.t

val embd_msg_rep :
  t ->
  tag ->
  'a list ->
  ('a -> (string, [> error ] as 'b) Result.t) ->
  (unit, 'b) Result.t
