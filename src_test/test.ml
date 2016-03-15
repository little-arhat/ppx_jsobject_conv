
open Result

type x = int * string * int [@@deriving jsobject]
type f = Af | Bf of string | Cf of int * string * int [@@deriving jsobject]
type a = Gt [@rename "$gt"]
       | Lt of string [@rename "$lt"]
       | Eq of x [@rename "$eq"] [@@deriving jsobject]
type d = D of (int * string) | E of (string * int) [@@deriving jsobject]


type maybe_int = int option [@@deriving jsobject]
type arr_float = float array [@@deriving jsobject]
type string_list = string list [@@deriving jsobject]

type status = [`Created | `Registered of int | `Deleted of int] [@@deriving jsobject]

type user = {
    age: int;
    name: string;
    status: status
  } [@@deriving jsobject]

let show_status = function
  | `Created -> "`Created"
  | `Registered(i) -> Printf.sprintf "`Registered(%d)" i
  | `Deleted(i) -> Printf.sprintf "`Deleted(%d)" i

module Nullable = struct
  type user = {
      age: int option;
      name: string option;
      status: status option
    } [@@deriving jsobject]

  let show_user = function
    | {age;name;status} ->
       let s_age = match age with
         | Some(a) -> Printf.sprintf "age=%d" a
         | None -> "" in
       let s_name = match name with
         | Some(n) -> Printf.sprintf "name=%s" n
         | None -> "" in
       let s_status = match status with
         | Some(s) -> Printf.sprintf "status=%s" (show_status s)
         | None -> "" in
       let f = String.concat ";" @@ List.filter
                                      (fun s -> String.length s != 0 )
                                      [s_age; s_name; s_status] in
       Printf.sprintf "{%s}" f
  end

type outside = Something of Nullable.user [@@deriving jsobject]

let json_parse json =
  let jclass = (Js.Unsafe.variable "JSON") in
  let jss = Js.string json in
  Js.Unsafe.meth_call jclass "parse" [| Js.Unsafe.inject jss |]


let show_user = function
  | {age;name;status} ->
     Printf.sprintf "{age=%d;name=%s;status=%s}" age name (show_status status)

(* trick ocamldep (maybe do this via myocamlbuild and _tags) *)
(* fix this *)
module Pjcr = Ppx_jsobject_conv_runtime

let ()=
  let open Result in
  let full_user = "{\"age\": 18, \"name\":\"Varya\", \"status\":[\"Created\"]}" in
  let partial_user1 = "{}" in
  let partial_user2 = "{\"age\": 24}" in
  let partial_user3 = "{\"age\": 12, \"name\":\"vasya\"}" in
  let fuo = json_parse full_user in
  let puo1 = json_parse partial_user1 in
  let puo2 = json_parse partial_user2 in
  let puo3 = json_parse partial_user3 in
  let () = Firebug.console##log (fuo) in
  let fu_res = user_of_jsobject_res fuo in
  let should_fail_fu_res = user_of_jsobject_res puo1 in
  let nu0_res = Nullable.user_of_jsobject_res fuo in
  let nu1_res = Nullable.user_of_jsobject_res puo1 in
  let nu2_res = Nullable.user_of_jsobject_res puo2 in
  let nu3_res = Nullable.user_of_jsobject_res puo3 in
  let () = match fu_res with
    | Ok(fu) -> Printf.printf "fuo OK: %s\n" (show_user fu)
    | Error(s) -> Printf.printf "fuo ERR: %s\n" (s)
  in
  let () = match should_fail_fu_res with
    | Ok(fu) -> Printf.printf "should_fail OK: %s\n" (show_user fu)
    | Error(s) -> Printf.printf "should_fail ERR: %s\n" (s)
  in
  let () = match nu0_res with
    | Ok(nu) -> Printf.printf "nu0 OK: %s\n" (Nullable.show_user nu)
    | Error(s) -> Printf.printf "nu0 ERR: %s\n" (s)
  in
  let () = match nu1_res with
    | Ok(nu) -> Printf.printf "nu1 OK: %s\n" (Nullable.show_user nu)
    | Error(s) -> Printf.printf "nu1 ERR: %s\n" (s)
  in
  let () = match nu2_res with
    | Ok(nu) -> Printf.printf "nu2 OK: %s\n" (Nullable.show_user nu)
    | Error(s) -> Printf.printf "nu2 ERR: %s\n" (s)
  in
  let () = match nu3_res with
    | Ok(nu) -> Printf.printf "nu3 OK: %s\n" (Nullable.show_user nu)
    | Error(s) -> Printf.printf "nu3 ERR: %s\n" (s)
  in
  ()
