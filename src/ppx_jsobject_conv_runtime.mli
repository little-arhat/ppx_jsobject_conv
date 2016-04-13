
(* module Js : (module type of Js) *)
(* module Result : (module type of Result) *)

include module type of Js
include module type of Result

type jsfunction

(* error handling *)
val (>|=) : ('a, 'b) Result.result -> ('a -> 'c) -> ('c, 'b) Result.result

val (>>=) : ('a, 'b) Result.result ->
            ('a -> ('c, 'b) Result.result) -> ('c, 'b) Result.result

val (>*=) : ('a, 'b) Result.result -> ('b -> 'c) -> ('a, 'c) Result.result

val concat_error_messages : string -> string -> string

(** of_jsobject *)
(* utilities *)

val is_object : 'a Js.t -> ('a Js.t, string) Result.result

val is_array : 'b Js.t -> ('a Js.t #Js.js_array Js.t, string) Result.result

val is_array_of_size_n :
  'a Js.t -> int -> ('b Js.t #Js.js_array Js.t, string) Result.result

val array_get_ind :
  'a #Js.js_array Js.t -> int -> ('a, string) Result.result
val object_get_key :
  'a Js.t -> string -> ('a Js.t, string) Result.result
val defined_or_error : 'a -> ('a, string) Result.result
val defined_or_default : ('a -> ('b, 'c) Result.result)
                         -> 'b -> 'a -> ('b, 'c) Result.result


(* std convs *)

val bool_of_jsobject : 'a Js.t -> (bool, string) Result.result
val unit_of_jsobject : 'a Js.t -> (unit, string) Result.result
val int_of_jsobject : 'a Js.t -> (int, string) Result.result
val float_of_jsobject : 'a Js.t -> (float, string) Result.result
val string_of_jsobject : 'a Js.t -> (string, string) Result.result
val option_of_jsobject :
  ('a -> ('b, 'c) Result.result) -> 'a -> ('b option, 'c) Result.result
val list_of_jsobject :
  ('a Js.t -> ('b, string) Result.result) ->
  'c Js.t -> ('b list, string) Result.result
val array_of_jsobject :
  ('a Js.t -> ('b, string) Result.result) ->
  'c Js.t -> ('b array, string) Result.result

val jsfunction_of_jsobject :
  'a Js.t -> (jsfunction, string) Result.result
val jst_of_jsobject : 'a Js.t -> ('b Js.t, string) Result.result
val jsany_of_jsobject : 'a Js.t -> (Js.Unsafe.any, string) Result.result

(** jsobject_of *)
(* utility conversions *)
val to_js_array : 'a list -> 'b Js.t
val make_jsobject : (string * 'm Js.t) array -> 'a Js.t

(* std convs *)
val jsobject_of_bool : bool -> 'm Js.t
val jsobject_of_int : int -> 'm Js.t
val jsobject_of_unit : unit -> 'm Js.t
val jsobject_of_string : string -> 'm Js.t
val jsobject_of_float : float -> 'm Js.t

val jsobject_of_option : ('a -> 'm Js.t) -> 'a option -> 'm Js.t
val jsobject_of_list: ('a -> 'm Js.t) -> 'a list -> 'n Js.t
val jsobject_of_array: ('a -> 'm Js.t) -> 'a array -> 'n Js.t

val jsobject_of_jsfunction : jsfunction -> 'm Js.t
val jsobject_of_jst : 'a Js.t -> 'm Js.t
val jsobject_of_jsany : 'm Js.t -> 'm Js.t
