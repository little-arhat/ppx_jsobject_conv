
type jsfunction = Js_of_ocaml.Js.Unsafe.any Js_of_ocaml.Js.t

(* error handling *)
val (>|=) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result

val (>>=) : ('a, 'b) result ->
            ('a -> ('c, 'b) result) -> ('c, 'b) result

val (>*=) : ('a, 'b) result -> ('b -> 'c) -> ('a, 'c) result

val concat_error_messages : string -> string -> string

val throw_js_error : string -> 'a

(** of_jsobject *)
(* utilities *)

val is_object : 'a Js_of_ocaml.Js.t -> ('a Js_of_ocaml.Js.t, string) result

val is_array : 'b Js_of_ocaml.Js.t -> ('a Js_of_ocaml.Js.t #Js_of_ocaml.Js.js_array Js_of_ocaml.Js.t, string) result

val is_array_of_size_n :
  'a Js_of_ocaml.Js.t -> int -> ('b Js_of_ocaml.Js.t #Js_of_ocaml.Js.js_array Js_of_ocaml.Js.t, string) result

val array_get_ind :
  'a #Js_of_ocaml.Js.js_array Js_of_ocaml.Js.t -> int -> ('a, string) result
val object_get_key :
  'a Js_of_ocaml.Js.t -> string -> ('a Js_of_ocaml.Js.t, string) result
val defined_or_error : 'a -> ('a, string) result
val defined_or_default : ('a -> ('b, 'c) result)
                         -> 'b -> 'a -> ('b, 'c) result
val convert_or_default : ('a -> ('b, 'c) result) -> 'b -> 'a -> ('b, 'd) result

(* std convs *)

val bool_of_jsobject : 'a Js_of_ocaml.Js.t -> (bool, string) result
val unit_of_jsobject : 'a Js_of_ocaml.Js.t -> (unit, string) result
val int_of_jsobject : 'a Js_of_ocaml.Js.t -> (int, string) result
val float_of_jsobject : 'a Js_of_ocaml.Js.t -> (float, string) result
val string_of_jsobject : 'a Js_of_ocaml.Js.t -> (string, string) result
val option_of_jsobject :
  ('a -> ('b, 'c) result) -> 'a -> ('b option, 'c) result
val list_of_jsobject :
  ('a Js_of_ocaml.Js.t -> ('b, string) result) ->
  'c Js_of_ocaml.Js.t -> ('b list, string) result
val array_of_jsobject :
  ('a Js_of_ocaml.Js.t -> ('b, string) result) ->
  'c Js_of_ocaml.Js.t -> ('b array, string) result

val object_get_sole_key : 'a Js_of_ocaml.Js.t -> (string, string) result

val jsfunction_of_jsobject :
  'a Js_of_ocaml.Js.t -> (jsfunction, string) result
val jst_of_jsobject : 'a Js_of_ocaml.Js.t -> ('b Js_of_ocaml.Js.t, string) result
val jsany_of_jsobject : 'a Js_of_ocaml.Js.t -> (Js_of_ocaml.Js.Unsafe.any, string) result

(** jsobject_of *)
(* utility conversions *)
val to_js_array : 'a list -> 'b Js_of_ocaml.Js.t
val make_jsobject : (string * 'm Js_of_ocaml.Js.t) array -> 'a Js_of_ocaml.Js.t
val make_jsobject_of_some : (string * 'm Js_of_ocaml.Js.t) option array -> 'a Js_of_ocaml.Js.t

(* std convs *)
val jsobject_of_bool : bool -> 'm Js_of_ocaml.Js.t
val jsobject_of_int : int -> 'm Js_of_ocaml.Js.t
val jsobject_of_unit : unit -> 'm Js_of_ocaml.Js.t
val jsobject_of_string : string -> 'm Js_of_ocaml.Js.t
val jsobject_of_float : float -> 'm Js_of_ocaml.Js.t

val jsobject_of_option : ('a -> 'm Js_of_ocaml.Js.t) -> 'a option -> 'm Js_of_ocaml.Js.t
val jsobject_of_list: ('a -> 'm Js_of_ocaml.Js.t) -> 'a list -> 'n Js_of_ocaml.Js.t
val jsobject_of_array: ('a -> 'm Js_of_ocaml.Js.t) -> 'a array -> 'n Js_of_ocaml.Js.t

val jsobject_of_jsfunction : jsfunction -> 'm Js_of_ocaml.Js.t
val jsobject_of_jst : 'a Js_of_ocaml.Js.t -> 'm Js_of_ocaml.Js.t
val jsobject_of_jsany : 'm Js_of_ocaml.Js.t -> 'm Js_of_ocaml.Js.t
