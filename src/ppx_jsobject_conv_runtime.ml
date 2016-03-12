
open StdLabels

module Js = Js
module Result = Result
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

(* of_jsobject *)
(* heplers *)

exception Short_circuit of string

let array_fold_right_short_circuit ~f arr ~init =
  try
    Ok(Array.fold_right
         ~f:(fun el acc ->
           match f el acc with
           | Ok(v) -> v
           | Error(s) -> raise @@ Short_circuit(s))
         ~init arr)
  with Short_circuit(s) -> Error(s)

let is_object v =
  result_of_bool (Js.typeof v = (Js.string "object"))
                 ("Expected object, got: " ^
                    (Js.to_bytestring @@ Js.typeof v))
  >|= (fun _ -> v)

let is_array v  =
  result_of_bool (Js.instanceof v Js.array_empty)
                 ("Expected array, got: " ^
                    (Js.to_bytestring @@ Js.typeof v))
  >|= (fun _ ->
    let arr:'a Js.t #Js.js_array Js.t = Js.Unsafe.coerce v
    in arr)

let array_length (arr : 'a Js.t #Js.js_array Js.t) : int =
  (Js.Unsafe.meth_call arr "length" [||])

let is_array_of_size_n obj expected =
  is_array obj >>=
    (fun arr ->
      let got = array_length arr in
      result_of_bool (expected = got)
                     (Printf.sprintf
                        "Expected array of length %d, got: %d"
                        expected got)
      >|= (fun _ -> arr))

let array_get_or_error arr ind =
  match Js.Optdef.to_option @@ Js.array_get arr ind with
  | Some v -> Ok(v)
  | None -> Error("Expceted value at index: " ^ (string_of_int ind))

let object_get_or_error (obj: 'a Js.t) (key:string) =
  let maybe_value:('a Js.t) Js.optdef = Js.Unsafe.get obj key in
  match Js.Optdef.to_option maybe_value with
  | Some(value) -> Ok(value)
  | None -> Error("Expected value by key: " ^ key)

(* conversion *)
let int_of_jsobject_res num =
  if Js.typeof num = (Js.string "number")
  then Ok(int_of_float @@
            Js.float_of_number @@
              Js.Unsafe.coerce num)
  else Error("not a number")

let float_of_jsobject_res num =
  if Js.typeof num = (Js.string "number")
  then Ok(Js.float_of_number @@
            Js.Unsafe.coerce num)
  else Error("not a number")

let string_of_jsobject_res st =
  if Js.typeof st = (Js.string "string")
  then Ok(Js.to_string (Js.Unsafe.coerce st))
  else Error("not a string")

let option_of_jsobject_res a__of_jsobject_res obj =
  match Js.Opt.to_option @@ Js.some obj with
  | Some(v) -> a__of_jsobject_res v >|= (fun i -> Some(i))
  | None -> Ok(None)

let list_of_jsobject_res a__of_jsobject_res obj =
  is_array obj >>=
    (fun arr ->
      let oarr = Js.to_array arr in
      array_fold_right_short_circuit
        ~f:(fun jsel l ->
          a__of_jsobject_res jsel
          >|= (fun oel -> oel::l))
        ~init:[]
        oarr
      >|= (fun l -> List.rev l))

let array_of_jsobject_res a__of_jsobject_res obj =
  list_of_jsobject_res a__of_jsobject_res obj >|= Array.of_list


(* jsobject_of *)
(* helpers *)
let inject o = Js.Unsafe.inject o

let new_array l =
  Js.Unsafe.new_obj Js.array_length [| inject l |]

let to_js_array l =
  let arr = new_array @@ List.length l in
  let set = Js.array_set arr in
  let () = List.iteri set l in
  arr

let make_jsobject pairs =
  inject @@ Js.Unsafe.obj @@ pairs

let number_of_int i = Js.number_of_float @@ float_of_int i

(* conversions *)

let jsobject_of_int v = inject @@ number_of_int v
let jsobject_of_string v = inject @@ Js.string v
let jsobject_of_float v = inject @@ Js.number_of_float v

let jsobject_of_option jsobject_of__a = function
  | Some(x) -> jsobject_of__a x
  | None -> inject @@ Js.null

let jsobject_of_list jsobject_of__a lst =
  to_js_array @@ List.rev  @@ List.rev_map jsobject_of__a lst
let jsobject_of_array jsobject_of__a arr =
  to_js_array @@ Array.to_list @@ Array.map jsobject_of__a arr
