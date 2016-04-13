
open StdLabels

include Js
include Result

type jsfunction = Js.Unsafe.any

let map f e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Result.Error s

let flat_map f e = match e with
  | Ok x -> f x
  | Error s -> Result.Error s

let map_err f e = match e with
  | Ok _ as res -> res
  | Error y -> Error (f y)

let (>|=) e f = map f e
let (>>=) e f = flat_map f e
let (>*=) e f = map_err f e

let result_of_bool v er = if v then Ok(v) else Error(er)

let string_typeof v =
  let tpof = Js.typeof v in
  if tpof = (Js.string "object")
  then (
    if Js.instanceof v Js.array_empty
    then "array"
    else (if Js.instanceof v (Js.Unsafe.get Js.Unsafe.global "String")
          then "string"
          else (if Js.Opt.test @@ Js.some v
                then "object"
                else "null")))
  else Js.to_bytestring tpof

let type_error v expected =
  Result.Error(Printf.sprintf "expected %s, got %s"
                              expected  (string_typeof v))
let concat_error_messages path msg =
  if String.contains msg ':'
  then path ^ "." ^ msg
  else path ^ ": " ^ msg

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
  let msg = Printf.sprintf "expected object, got %s" (string_typeof v) in
  result_of_bool (string_typeof v = "object") msg
  >|= (fun _ -> v)

let is_array v  =
  let msg = Printf.sprintf "expected array, got %s" (string_typeof v) in
  result_of_bool (Js.instanceof v Js.array_empty) msg
  >|= (fun _ ->
    let arr:'a Js.t #Js.js_array Js.t = Js.Unsafe.coerce v
    in arr)

let array_length_f (arr : 'a Js.t #Js.js_array Js.t) : int =
  (Js.Unsafe.meth_call arr "length" [||])

let is_array_of_size_n obj expected =
  is_array obj >>=
    (fun arr ->
      let got = array_length_f arr in
      result_of_bool (expected = got)
                     (Printf.sprintf
                        "expected array of length %d, got %d"
                        expected got)
      >|= (fun _ -> arr))

let array_get_ind arr ind =
  match Js.Optdef.to_option @@ Js.array_get arr ind with
  | Some v -> Ok(v)
  | None -> Ok(Js.Unsafe.eval_string("undefined"))

let object_get_key (obj: 'a Js.t) (key:string) =
  Ok(Js.Unsafe.get obj key)

let defined_or_error obj =
  match Js.Optdef.to_option @@ Js.def obj with
  | Some(o) -> Ok(o)
  | None -> Result.Error("expected value, got undefined")

let defined_or_default a__of_jsobject dflt obj =
  match Js.Optdef.to_option @@ Js.def obj with
  | Some(v) -> a__of_jsobject v
  | None -> Ok(dflt)

(* conversion *)
let bool_of_jsobject obj =
  if string_typeof obj = "boolean"
  then Ok(Js.to_bool @@ Js.Unsafe.coerce obj)
  else type_error obj "boolean"

let unit_of_jsobject obj =
  if string_typeof obj = "undefined"
  then Ok(())
  else type_error obj "undefined"

let int_of_jsobject obj =
  if string_typeof obj = "number"
  then Ok(int_of_float @@
            (* TODO: check for "int-nesses" *)
            Js.float_of_number @@
              Js.Unsafe.coerce obj)
  else type_error obj "number"

let float_of_jsobject obj =
  if string_typeof obj = "number"
  then Ok(Js.float_of_number @@
            Js.Unsafe.coerce obj)
  else type_error obj "number"

let string_of_jsobject obj =
  if string_typeof obj = "string"
  then Ok(Js.to_string (Js.Unsafe.coerce obj))
  else type_error obj "string"

let option_of_jsobject a__of_jsobject obj =
  match Js.Optdef.to_option @@ Js.def obj with
  | Some(v) -> (match Js.Opt.to_option @@ Js.some v with
                | Some(v') ->a__of_jsobject v' >|= (fun i -> Some(i))
                | None -> Ok(None)
               )
  | None -> Ok(None)

let list_of_jsobject a__of_jsobject obj =
  is_array obj >>=
    (fun arr ->
      let oarr = Js.to_array arr in
      array_fold_right_short_circuit
        ~f:(fun jsel l ->
          a__of_jsobject jsel
          >|= (fun oel -> oel::l))
        ~init:[]
        oarr
      >|= (fun l -> List.rev l))

let array_of_jsobject a__of_jsobject obj =
  list_of_jsobject a__of_jsobject obj >|= Array.of_list

let jsfunction_of_jsobject obj =
  if string_typeof obj = "function"
  then Ok(Obj.magic obj)
  else type_error obj "function"

let jst_of_jsobject obj =
  Ok(Obj.magic obj)

(* jsobject_of *)
(* helpers *)
let inject o = Js.Unsafe.inject o

let new_array l =
  Js.Unsafe.new_obj Js.array_length [| inject l |]

let to_js_array l =
  let arr = new_array @@ List.length l in
  let set = Js.array_set arr in
  let () = List.iteri ~f:set l in
  arr

let make_jsobject pairs =
  inject @@ Js.Unsafe.obj @@ pairs

let number_of_int i = Js.number_of_float @@ float_of_int i

(* conversions *)

let jsobject_of_bool v = inject @@ Js.bool v
let jsobject_of_unit () = inject @@ Js.undefined
let jsobject_of_int v = inject @@ number_of_int v
let jsobject_of_string v = inject @@ Js.string v
let jsobject_of_float v = inject @@ Js.number_of_float v

let jsobject_of_option jsobject_of__a = function
  | Some(x) -> jsobject_of__a x
  | None -> inject @@ Js.null

let jsobject_of_list jsobject_of__a lst =
  to_js_array @@ List.rev  @@ List.rev_map ~f:jsobject_of__a lst
let jsobject_of_array jsobject_of__a arr =
  to_js_array @@ Array.to_list @@ Array.map ~f:jsobject_of__a arr

let jsobject_of_jsfunction v = inject v

let jsobject_of_jst v = inject v
