
(* trick ocamldep (maybe do this via myocamlbuild and _tags) *)
(* fix this *)
module Pjcr = Ppx_jsobject_conv_runtime

(* XXX: clashes with Webtest.Suite.result :/ *)
open Webtest

module JSON = struct
  let json = (Js.Unsafe.variable "JSON")

  let parse j =
    let jss = Js.string j in
    Js.Unsafe.meth_call json "parse" [| Js.Unsafe.inject jss |]

  let stringify obj =
    Js.to_string @@ Js.Unsafe.meth_call json "stringify" [| Js.Unsafe.inject obj |]
end

let expect_ok = function
  | Ok _ -> true
  | _ -> false
let expect_error = function
  | Ok _ -> false
  | Error _ -> true
let unwrap = function
  | Ok x -> x
  | _ -> failwith "unwrap"

let tests = ref []

let _should_parse : 'a . string ->
                    ?compare_with:string ->
                    (Js.Unsafe.any Js.t -> ('a, string) result) ->
                    ('a -> Js.Unsafe.any Js.t) -> unit
  = fun inp ?compare_with from_jsobject to_jsobject ->
  let parsed = JSON.parse inp in
  let result = from_jsobject parsed in
  let () = Suite.assert_true @@ expect_ok result in
  let generated = JSON.stringify @@ to_jsobject @@ unwrap result in
  let compare_with = match compare_with with
    | Some cw -> cw
    | None -> inp
  in
  (* XXX: comparison of generated text is quite fragile
          maybe use structures instead *)
  Suite.assert_equal ~printer:(fun a -> a) generated compare_with
let should_parse name input ?compare_with from_jsobject to_jsobject =
  let open Suite in
  let msg = Printf.sprintf "will parse %s '%s'" name input in
  let t = msg >:: (fun _ -> _should_parse input ?compare_with
                                          from_jsobject to_jsobject) in
  tests := (t::!tests)

let _should_fail : 'a . string ->
                   (Js.Unsafe.any Js.t -> ('a, string) result) ->
                   unit =
  fun inp from_jsobject ->
  let parsed = JSON.parse inp in
  let result = from_jsobject parsed in
  (* XXX: check error message as well? *)
  Suite.assert_true @@ expect_error result
let should_fail name input from_jsobject  =
  let open Suite in
  let msg = Printf.sprintf "will not parse %s '%s'" name input in
  let t = msg >:: (fun _ -> _should_fail input from_jsobject) in
  tests := (t::!tests)


type simple_tuple = int * string * int [@@deriving jsobject]
let () = should_parse "simple tuple" "[1,\"Some\",42]"
                      simple_tuple_of_jsobject
                      jsobject_of_simple_tuple

type simple_enum = Af | Bf of string | Cf of int * string * int [@@deriving jsobject]
let () = should_parse "simple enum"
                      "[\"Bf\",\"something\"]"
                      simple_enum_of_jsobject
                      jsobject_of_simple_enum

type variant_rename = Gt [@name "$gt"]
                    | Lt of string [@name "$lt"]
                    | Eq of simple_tuple [@name "$eq"] [@@deriving jsobject]
let () = should_parse "variant rename" "[\"$gt\"]"
                      variant_rename_of_jsobject
                      jsobject_of_variant_rename;
         should_fail "variant rename with raw name" "[\"Lt\",\"oops\"]"
                     variant_rename_of_jsobject

type inline_tuple = D of (int * string)
                    | E of (string * int) [@@deriving jsobject]
let () = should_parse "inline tuple" "[\"D\",[42,\"something\"]]"
                      inline_tuple_of_jsobject
                      jsobject_of_inline_tuple

type field_rename = {
    field_name: string [@key "FieldName"]
  } [@@deriving jsobject]
let () = should_parse "field rename" {|{"FieldName":"something"}|}
                      field_rename_of_jsobject
                      jsobject_of_field_rename;
         should_fail "field rename with raw name" {|{"field_name":"something"}|}
                     field_rename_of_jsobject

type maybe_int = int option [@@deriving jsobject]
let () = should_parse "maybe int" "32"
                      maybe_int_of_jsobject
                      jsobject_of_maybe_int;
         should_parse "maybe int" "null"
                      maybe_int_of_jsobject
                      jsobject_of_maybe_int

type arr_float = float array [@@deriving jsobject]
let () = should_parse "float array" "[4.2,2,5.3,1.24e+24]"
                      arr_float_of_jsobject
                      jsobject_of_arr_float

type string_list = string list [@@deriving jsobject]
let () = should_parse "string list" {|["some","quick","по-русски"]|}
                      string_list_of_jsobject
                      jsobject_of_string_list

type status = [`Created |
               `Registered of int * string |
               `Deleted of int] [@@deriving jsobject]
type user = {
    age: int;
    name: string;
    status: status
  } [@@deriving jsobject]
module NullableUser = struct
  type t = {
      age: int option;
      name: string option;
      status: status option
    } [@@deriving jsobject]
end
let full_user = "{\"age\":18,\"name\":\"Varya\",\"status\":[\"Created\"]}"
let () = should_parse "user" full_user
                      user_of_jsobject
                      jsobject_of_user;
         should_parse "nullable user" full_user
                      NullableUser.of_jsobject
                      NullableUser.jsobject_of;
         should_parse "nullable user" "{}"
                      ~compare_with:{|{"age":null,"name":null,"status":null}|}
                      NullableUser.of_jsobject
                      NullableUser.jsobject_of;
         should_parse "nullable user" "{\"age\":24}"
                      ~compare_with:{|{"age":24,"name":null,"status":null}|}
                      NullableUser.of_jsobject
                      NullableUser.jsobject_of
let partial_user3 = "{\"age\":12,\"name\":\"vasya\"}"
let () = should_parse "nullable user" partial_user3
                      ~compare_with:{|{"age":12,"name":"vasya","status":null}|}
                      NullableUser.of_jsobject
                      NullableUser.jsobject_of;
         should_fail "user" partial_user3
                     user_of_jsobject

type outside_of_module = Something of NullableUser.t [@@deriving jsobject]
let () = should_parse "outside of module" "[\"Something\",{\"age\":13}]"
                      ~compare_with:{|["Something",{"age":13,"name":null,"status":null}]|}
                      outside_of_module_of_jsobject
                      jsobject_of_outside_of_module

type condition = Gt of int | Lt of int [@@deriving jsobject]
type query = {amount: float; condition: condition} [@@deriving jsobject]
type basket = {name: string; query: query} [@@deriving jsobject]
type message = Basket of basket | Nop [@@deriving jsobject]
type command = {message: message} [@@deriving jsobject]
let command_good = {|{"message":["Basket",{"name":"basket","query":{|} ^
                    {|"amount":66.6,"condition":["Gt",30]}}]}|}
let command_bad_nested_error = {|{"message":["Basket",{"name":"basket","query":{|} ^
                               {|"amount":66.6,"condition":["Eq",30]}}]}|}
let () = should_parse "command" command_good
                      command_of_jsobject
                      jsobject_of_command;
         should_fail "command with nested error" command_bad_nested_error
                     command_of_jsobject

type with_defaults = {def_cond: condition [@default Gt(32)];
                      enabled: bool [@default true];
                      kind: string [@default "integer"]} [@@deriving jsobject]
let () = should_parse "with_default from empty" "{}"
                      ~compare_with:{|{"def_cond":["Gt",32],"enabled":true,"kind":"integer"}|}
                      with_defaults_of_jsobject
                      jsobject_of_with_defaults;
         should_parse "with_default 2" "{\"kind\":\"normal\"}"
                      ~compare_with:{|{"def_cond":["Gt",32],"enabled":true,"kind":"normal"}|}
                      with_defaults_of_jsobject
                      jsobject_of_with_defaults;
         should_parse "with_default from full"
                      {|{"def_cond":["Gt",32],"enabled":true,"kind":"normal"}|}
                      with_defaults_of_jsobject
                      jsobject_of_with_defaults;
         should_fail "with defaults invalid"
                     "{\"def_cond\":\"invalid input\",\"kind\":\"normal\"}"
                     with_defaults_of_jsobject

type err_default = {something: float [@jsobject.default_on_error 0.0];
                    query: query option [@jsobject.default_on_error None]
                   } [@@deriving jsobject]
let () = should_parse "default on error"
                      {|{"something":"invalid","query":1}|}
                      ~compare_with:{|{"something":0,"query":null}|}
                      err_default_of_jsobject
                      jsobject_of_err_default

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
type email_info = {email: Email.t} [@@deriving jsobject]
let () = should_parse "email" "{\"email\":\"some@example.org\"}"
                      email_info_of_jsobject
                      jsobject_of_email_info;
         should_fail "invalid email" "{\"email\":\"someexample.org\"}"
                     email_info_of_jsobject

type variant_as_object = Gtn of int [@name "$gt"] [@jsobject.sum_type_as "object"]
                       | Ltn of int [@name "$lt"]  [@@deriving jsobject]
let () = should_parse "variant as object"
                      {|{"$lt":24}|}
                      variant_as_object_of_jsobject
                      jsobject_of_variant_as_object;
         should_fail "invalid variant name" "{\"garbage\":12}"
                     variant_as_object_of_jsobject;
         should_fail "invalid value" "{\"$gt\":\"something\"}"
                     variant_as_object_of_jsobject

type enum = Var1 [@name "var1"] | Var2 | Var3 [@sum_type_as "enum"] [@@deriving jsobject]
type enum_info = {enum: enum} [@@deriving jsobject]
let () = should_parse "enum" "{\"enum\":\"var1\"}"
                      enum_info_of_jsobject
                      jsobject_of_enum_info;
         should_parse "enum" "{\"enum\":\"Var3\"}"
                      enum_info_of_jsobject
                      jsobject_of_enum_info;
         should_fail "enum with invalid variant"
                     "{\"enum\":\"ftwf\"}"
                      enum_info_of_jsobject

type ('a,'b) type_parameters = {data: 'a; ident: 'b; kind: string} [@@deriving jsobject]
type some_detais = {details: string} [@@deriving jsobject]
type some_ident = string [@@deriving jsobject]
type parametrized = (some_detais, some_ident) type_parameters [@@deriving jsobject]
let () =
  should_parse "parametrized"
               "{\"data\":{\"details\":\"fond\"},\"ident\":\"some\",\"kind\":\"normal\"}"
               parametrized_of_jsobject
               jsobject_of_parametrized;
  should_parse "parametrized with implicit int to string conv"
               "{\"data\":{\"details\":33},\"ident\":\"some\",\"kind\":\"normal\"}"
               ~compare_with:"{\"data\":{\"details\":\"33\"},\"ident\":\"some\",\"kind\":\"normal\"}"
               parametrized_of_jsobject
               jsobject_of_parametrized

type tagless =  U2 of {inlinef: float; inlines: string}
              | U1 of user [@jsobject.sum_type_as "tagless"]
                           [@@deriving jsobject]
let () =
  should_parse "tagless u1" full_user
               tagless_of_jsobject
               jsobject_of_tagless;
  should_parse "tagless u2" "{\"inlinef\":12.4,\"inlines\":\"yes\"}"
               tagless_of_jsobject
               jsobject_of_tagless;
  should_fail "invalid tagless" "{\"enum\":\"ftwf\"}"
              tagless_of_jsobject

type drop_none = {some: string option [@jsobject.drop_none]}
                   [@@deriving jsobject]
let () =
  should_parse "drop_none with null" "{\"some\":null}"
               ~compare_with:"{}"
               drop_none_of_jsobject
               jsobject_of_drop_none;
  should_parse "drop_none without null" "{\"some\":\"thing\"}"
               drop_none_of_jsobject
               jsobject_of_drop_none

module ForOpen = struct
  type open_type = .. [@@deriving jsobject]
end
type ForOpen.open_type += OpV1 [@@deriving jsobject]
type ForOpen.open_type += OpV2 [@@deriving jsobject]
let () =
  should_parse "open type 1" "[\"OpV1\"]"
               ForOpen.open_type_of_jsobject
               ForOpen.jsobject_of_open_type;
  should_fail "invalid open type" "[\"Invalid\"]"
               ForOpen.open_type_of_jsobject

let suite = Suite.(>:::) "conv" (List.rev !tests)

let () =
  Webtest_js.Runner.run suite


(* type recfs = { *)
(*     func: Ppx_jsobject_conv_runtime.jsfunction *)
(*   } [@@deriving jsobject] *)

(* type noop = {carry: int Js.t; ident: string} [@@deriving jsobject] *)
(* type anoop = {acarry: Js.Unsafe.any Js.t; aident: string} [@@deriving jsobject] *)
(* type test_unit = (unit * unit) [@@deriving jsobject] *)
(* type test_any = Js.Unsafe.any * int [@@deriving jsobject] *)
