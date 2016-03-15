(* Since we open Ppx_jsobject_conv only to trick ocamldep, we want to
   silence this warning
*)
[@@@ocaml.warning "-33"]
open Ppx_jsobject_conv

let () =
  Ppx_driver.standalone ()
