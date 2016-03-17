
open Result

type x = int * string * int [@@deriving jsobject]
type f = Af | Bf of string | Cf of int * string * int [@@deriving jsobject]
type a = Gt [@name "$gt"]
       | Lt of string [@name "$lt"]
       | Eq of x [@name "$eq"] [@@deriving jsobject]
type d = D of (int * string) | E of (string * int) [@@deriving jsobject]

type go_style_struct = {
    field_name: string [@key "FieldName"]
  } [@@deriving jsobject]
let show_go_style_struct = function
  | {field_name} -> Printf.sprintf "{field_name=%s}" field_name


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

type condition = Gt of int | Lt of int [@@deriving jsobject]
let show_condition = function
  | Gt(i) -> Printf.sprintf "Gt(%d)" i
  | Lt(i) -> Printf.sprintf "Lt(%d)" i

type query = {amount: float; condition: condition} [@@deriving jsobject]
let show_query = function
  | {amount;condition} -> Printf.sprintf "{amount=%f;condition=%s}"
                                         amount (show_condition condition)

type basket = {name: string; query: query} [@@deriving jsobject]
let show_basket = function
  | {name;query} -> Printf.sprintf "{name=%s;query=%s}" name (show_query query)

type message = Basket of basket | Nop [@@deriving jsobject]
let show_message = function
  | Basket(b) -> Printf.sprintf "Basket(%s)" (show_basket b)
  | Nop -> "Nop"

type command = {message: message} [@@deriving jsobject]
let show_command = function
  | {message} -> Printf.sprintf "{message=%s}" (show_message message)

module M = struct
  type t = int [@@deriving jsobject]
end
type out = Other of M.t [@@deriving jsobject]

module JSON = struct
  let json = (Js.Unsafe.variable "JSON")

  let parse j =
    let jss = Js.string j in
    Js.Unsafe.meth_call json "parse" [| Js.Unsafe.inject jss |]

  let stringify obj =
    Js.Unsafe.meth_call json "stringify" [| obj |]
end


let show_user = function
  | {age;name;status} ->
     Printf.sprintf "{age=%d;name=%s;status=%s}" age name (show_status status)

(* trick ocamldep (maybe do this via myocamlbuild and _tags) *)
(* fix this *)
module Pjcr = Ppx_jsobject_conv_runtime

module Email = struct
  type t = string [@@deriving jsobject]
  let of_jsobject o =
    let open! Ppx_jsobject_conv_runtime in
    of_jsobject o
    >>= (fun s ->
      if String.contains s '@'
      then Ok(s)
      else Error("expected email, got random string"))
  let show e = e
end
type email_info = {
    email: Email.t
  } [@@deriving jsobject]
let show_email_info ei = Printf.sprintf "{email=%s}" (Email.show ei.email)

let run_test name inp conv_func show_func =
  (* add "expected" flag or use ounit *)
  let parsed = JSON.parse inp in
  match conv_func parsed with
  | Ok(converted) ->
     Printf.printf "OK [%s]: %s  --> %s\n" name inp (show_func converted)
  | Error(msg) ->
     Printf.printf "ERR [%s]: %s --> %s\n" name inp msg

let ()=
  let full_user = "{\"age\": 18, \"name\":\"Varya\", \"status\":[\"Created\"]}" in
  let partial_user1 = "{}" in
  let partial_user2 = "{\"age\": 24}" in
  let partial_user3 = "{\"age\": 12, \"name\":\"vasya\"}" in
  let faulty_full_user = "{\"age\": 18, \"status\":[\"Created\"]}" in
  let go_style_struct = "{\"FieldName\": \"some field name\"}" in
  let command_json1 = {sm|
{"message": [
             "Basket",
             {
                 "name": "basket",
                 "query": {
                     "amount": 66.6,
                     "condition": [
                         "Eq", 30
                     ]
                 }
             }
           ]}
                       |sm} in
  let command_json2 = {sm|
{"message": [
             "Basket",
             {
                 "name": "basket",
                 "query": {
                     "amount": 66.6,
                     "condition": [
                         "Gt", 30
                     ]
                 }
             }
           ]}
                       |sm} in
  let basket_json = {|
{"name": "name", "query": {"amount": "no", "condition": ["Lt", 1]}}
                     |} in
  run_test "full_user" full_user user_of_jsobject show_user;
  run_test "partial1" partial_user1
           Nullable.user_of_jsobject Nullable.show_user;
  run_test "partial2" partial_user2
           Nullable.user_of_jsobject Nullable.show_user;
  run_test "partial3" partial_user3
           Nullable.user_of_jsobject Nullable.show_user;
  run_test "faulty" faulty_full_user
           user_of_jsobject show_user;
  run_test "go_style" go_style_struct
           go_style_struct_of_jsobject show_go_style_struct;
  run_test "command1" command_json1 command_of_jsobject show_command;
  run_test "command2" command_json2 command_of_jsobject show_command;
  run_test "basket1" basket_json basket_of_jsobject show_basket;
  run_test "email1" "{\"email\":\"some@example.org\"}" email_info_of_jsobject show_email_info;
  run_test "email2" "{\"email\":\"someexample.org\"}" email_info_of_jsobject show_email_info;
