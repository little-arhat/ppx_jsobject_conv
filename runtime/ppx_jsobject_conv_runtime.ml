
open StdLabels

type jsfunction = Js_of_ocaml.Js.Unsafe.any Js_of_ocaml.Js.t

let map f e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Error s

let flat_map f e = match e with
  | Ok x -> f x
  | Error s -> Error s

let map_err f e = match e with
  | Ok _ as res -> res
  | Error y -> Error (f y)

let (>|=) e f = map f e
let (>>=) e f = flat_map f e
let (>*=) e f = map_err f e

let result_of_bool v er = if v then Ok(v) else Error(er)

let throw_js_error msg =
  let err = Js_of_ocaml.Js.Unsafe.new_obj Js_of_ocaml.Js.error_constr [|Js_of_ocaml.Js.Unsafe.inject @@
                                                Js_of_ocaml.Js.string msg|] in
  Js_of_ocaml.Js.raise_js_error err

let string_typeof v =
  let tpof = Js_of_ocaml.Js.typeof v in
  if tpof = (Js_of_ocaml.Js.string "object")
  then (
    if Js_of_ocaml.Js.instanceof v Js_of_ocaml.Js.array_empty
    then "array"
    else (if Js_of_ocaml.Js.instanceof v (Js_of_ocaml.Js.Unsafe.get
                                            Js_of_ocaml.Js.Unsafe.global
                                            (Js_of_ocaml.Js.string "String"))
          then "string"
          else (if Js_of_ocaml.Js.Opt.test @@ Js_of_ocaml.Js.some v
                then "object"
                else "null")))
  else Js_of_ocaml.Js.to_string tpof

let type_error v expected =
  Error(Printf.sprintf "expected %s, got %s"
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
    Ok(snd @@ Array.fold_right
         ~f:(fun el (i, acc) ->
           match f i el acc with
           | Ok(v) -> (i + 1, v)
           | Error(s) -> raise @@ Short_circuit(s))
         ~init:(0, init) arr)
  with Short_circuit(s) -> Error(s)

let is_object v =
  let msg = Printf.sprintf "expected object, got %s" (string_typeof v) in
  result_of_bool (string_typeof v = "object") msg
  >|= (fun _ -> v)

let is_array v  =
  let msg = Printf.sprintf "expected array, got %s" (string_typeof v) in
  result_of_bool (Js_of_ocaml.Js.instanceof v Js_of_ocaml.Js.array_empty) msg
  >|= (fun _ ->
    let arr:'a Js_of_ocaml.Js.t #Js_of_ocaml.Js.js_array Js_of_ocaml.Js.t = Js_of_ocaml.Js.Unsafe.coerce v
    in arr)

let array_length_f (arr : 'a Js_of_ocaml.Js.t #Js_of_ocaml.Js.js_array Js_of_ocaml.Js.t) : int =
  (Js_of_ocaml.Js.Unsafe.get arr (Js_of_ocaml.Js.string "length"))

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
  match Js_of_ocaml.Js.Optdef.to_option @@ Js_of_ocaml.Js.array_get arr ind with
  | Some v -> Ok(v)
  | None -> Ok(Js_of_ocaml.Js.Unsafe.eval_string("undefined"))

let object_get_key (obj: 'a Js_of_ocaml.Js.t) (key:string) =
  Ok(Js_of_ocaml.Js.Unsafe.get obj (Js_of_ocaml.Js.string key))

let defined_or_error obj =
  match Js_of_ocaml.Js.Optdef.to_option @@ Js_of_ocaml.Js.def obj with
  | Some(o) -> Ok(o)
  | None -> Error("expected value, got undefined")

let defined_or_default a__of_jsobject dflt obj =
  match Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Js.some obj with
  | Some(v) -> a__of_jsobject v
  | None -> Ok(dflt)

let convert_or_default a__of__jsobject default obj =
  match a__of__jsobject obj with
  | Error _ -> Ok default
  | Ok _ as ok -> ok

(* conversion *)
let bool_of_jsobject obj =
  if string_typeof obj = "boolean"
  then Ok(Js_of_ocaml.Js.to_bool @@ Js_of_ocaml.Js.Unsafe.coerce obj)
  else type_error obj "boolean"

let unit_of_jsobject obj =
  if string_typeof obj = "undefined"
  then Ok(())
  else type_error obj "undefined"

let int_of_jsobject obj =
  if string_typeof obj = "number"
  then Ok(int_of_float @@
            (* TODO: check for "int-nesses" *)
            Js_of_ocaml.Js.float_of_number @@
              Js_of_ocaml.Js.Unsafe.coerce obj)
  else type_error obj "number"

let float_of_jsobject obj =
  if string_typeof obj = "number"
  then Ok(Js_of_ocaml.Js.float_of_number @@
            Js_of_ocaml.Js.Unsafe.coerce obj)
  else type_error obj "number"

let string_of_jsobject obj =
  (* XXX: should we have an option for such liberal conversion? *)
  if (string_typeof obj = "string")
  then Ok(Js_of_ocaml.Js.to_string (Js_of_ocaml.Js.Unsafe.coerce obj))
  else (if (string_typeof obj = "number")
        then Ok(string_of_int @@ int_of_float @@
                  Js_of_ocaml.Js.float_of_number @@ Js_of_ocaml.Js.Unsafe.coerce obj)
        else type_error obj "string")

let option_of_jsobject a__of_jsobject obj =
  match Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Js.some obj with
  | Some(v) -> (match Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Js.some v with
                | Some(v') ->a__of_jsobject v' >|= (fun i -> Some(i))
                | None -> Ok(None)
               )
  | None -> Ok(None)

let list_of_jsobject a__of_jsobject obj =
  is_array obj >>=
    (fun arr ->
      let oarr = Js_of_ocaml.Js.to_array arr in
      array_fold_right_short_circuit
        ~f:(fun i jsel l ->
          a__of_jsobject jsel
          >*= (fun emsg -> concat_error_messages (string_of_int i) emsg)
          >|= (fun oel -> oel::l))
        ~init:[]
        oarr)

let object_get_sole_key (obj: 'a Js_of_ocaml.Js.t) =
  let keys = Js_of_ocaml.Js.object_keys obj in
  is_array_of_size_n keys 1
  >*= (fun e -> "Object keys -- " ^ e)
  >>= (fun ar -> array_get_ind ar 0)
  >>= string_of_jsobject

let array_of_jsobject a__of_jsobject obj =
  list_of_jsobject a__of_jsobject obj >|= Array.of_list

let jsfunction_of_jsobject obj =
  if string_typeof obj = "function"
  then Ok(Obj.magic obj)
  else type_error obj "function"

let jst_of_jsobject obj =
  Ok(Obj.magic obj)

let jsany_of_jsobject obj =
  Ok(Obj.magic obj)

(* jsobject_of *)
(* helpers *)
let inject o = Js_of_ocaml.Js.Unsafe.inject o

let new_array l =
  Js_of_ocaml.Js.Unsafe.new_obj Js_of_ocaml.Js.array_length [| inject l |]

let to_js_array l =
  let arr = new_array @@ List.length l in
  let set = Js_of_ocaml.Js.array_set arr in
  let () = List.iteri ~f:set l in
  arr

let make_jsobject pairs =
  Js_of_ocaml.Js.Unsafe.obj @@ Array.map pairs ~f:(fun (k, v)-> (k, inject v))

let make_jsobject_of_some pairs =
  let unwrap_some l = Array.of_list @@ List.rev @@
                        Array.fold_left l ~init:[]
                                        ~f:(fun acc i ->
                                          match i with
                                          | None -> acc
                                          | Some c -> (c::acc))
  in
  make_jsobject @@ unwrap_some pairs

let number_of_int i = Js_of_ocaml.Js.number_of_float @@ float_of_int i

(* conversions *)

let jsobject_of_bool v = Js_of_ocaml.Js.Unsafe.coerce @@ Js_of_ocaml.Js.bool v
let jsobject_of_unit () = Obj.magic Js_of_ocaml.Js.undefined
let jsobject_of_int v = Js_of_ocaml.Js.Unsafe.coerce @@ number_of_int v
let jsobject_of_string v = Js_of_ocaml.Js.Unsafe.coerce @@ Js_of_ocaml.Js.string v
let jsobject_of_float v = Js_of_ocaml.Js.Unsafe.coerce @@ Js_of_ocaml.Js.number_of_float v

let jsobject_of_option jsobject_of__a = function
  | Some(x) -> jsobject_of__a x
  | None -> Obj.magic Js_of_ocaml.Js.null

let jsobject_of_list jsobject_of__a lst =
  to_js_array @@ List.rev  @@ List.rev_map ~f:jsobject_of__a lst
let jsobject_of_array jsobject_of__a arr =
  to_js_array @@ Array.to_list @@ Array.map ~f:jsobject_of__a arr

let jsobject_of_jsfunction v = Obj.magic v

let jsobject_of_jst v = Js_of_ocaml.Js.Unsafe.coerce v
let jsobject_of_jsany v = v
