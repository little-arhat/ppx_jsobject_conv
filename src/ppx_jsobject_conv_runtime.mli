

(* error handling *)
val (>|=) : ('a, 'b) Result.result -> ('a -> 'c) -> ('c, 'b) Result.result

val (>>=) : ('a, 'b) Result.result ->
            ('a -> ('c, 'b) Result.result) -> ('c, 'b) Result.result

(* utility conversions *)
val to_js_array : 'a list -> Js.Unsafe.any
val make_jsobject : (string * Js.Unsafe.any) array -> Js.Unsafe.any

(* std convs *)
val jsobject_of_int : int -> Js.Unsafe.any
val jsobject_of_string : string -> Js.Unsafe.any
val jsobject_of_float : float -> Js.Unsafe.any

val jsobject_of_option : ('a -> Js.Unsafe.any) -> 'a option -> Js.Unsafe.any
val jsobject_of_list: ('a -> Js.Unsafe.any) -> 'a list -> Js.Unsafe.any
val jsobject_of_array: ('a -> Js.Unsafe.any) -> 'a array -> Js.Unsafe.any

module Js : (module type of Js)
module Result : (module type of Result)
