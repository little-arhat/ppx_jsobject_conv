
open Result

let map f e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Error s

let flat_map f e = match e with
  | Ok x -> f x
  | Error s -> Error s

let (>|=) e f = map f e

let (>>=) e f = flat_map f e

let result_of_bool v er = if v then Ok(v) else Error(er)


(* js helpers *)
let inject o = Js.Unsafe.inject o

let number_of_int i = Js.number_of_float @@ float_of_int i

let int_of_number_res num =
  if Js.typeof num = (Js.string "number")
  then Ok(int_of_float @@
            Js.float_of_number @@
              Js.Unsafe.coerce num)
  else Error("not a number")

let string_of_jsstring_res st =
  if Js.typeof st = (Js.string "string")
  then Ok(Js.to_string (Js.Unsafe.coerce st))
  else Error("not a string")

(* let new_array () = *)
(*   let arrconst = Js.array_empty in *)
(*   new%js arrconst *)

(* let array_push el arr = *)
(*   let _:int = arr##push el in *)
(*   arr *)

let to_array l =
  let arrconst_n = Js.array_length in
  let arr = new%js arrconst_n (List.length l) in
  let set = Js.array_set arr in
  let () = List.iteri set l in
  arr

let jsobject_of_int = inject @@ number_of_int
let jsobject_of_string = inject @@ Js.string
let jsobject_of_float = inject @@ Js.number_of_float

let jsobject_of_option jsobject_of__a = function
  | Some(x) -> jsobject_of__a x
  | None -> inject @@ Js.null

let jsobject_of_list jsobject_of__a lst =
  to_array @@ List.rev  @@ List.rev_map jsobject_of__a lst
let jsobject_of_array jsobject_of__a arr =
  to_array @@ Array.to_list @@ Array.map jsobject_of__a arr

let make_jsobject_from_list pairs =
  inject @@ Js.Unsafe.obj @@ Array.of_list pairs

module Js = Js
