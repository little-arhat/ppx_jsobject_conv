open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
     flag ["ocaml"; "compile"; "use_ppx_jsobject_conv"] &
       S[A"-ppx"; A("./bin/ppx_jsobject.byte -as-ppx")];
     ocaml_lib ~dir:"src" "src/ppx_jsobject_conv_runtime";
     (* Pass -predicates to ocamldep *)
     pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])

  | _ -> ())
