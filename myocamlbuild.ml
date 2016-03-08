open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    ocaml_lib ~dir:"src" "src/ppx_jsobject_conv_runtime";
    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])

  | _ -> ())
