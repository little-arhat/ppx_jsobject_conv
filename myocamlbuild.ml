open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
     flag ["ocaml"; "compile"; "use_ppx_jsobject_conv"] &
       S[A"-ppx"; A("./bin/ppx_jsobject.byte -as-ppx")];
     (* flag ["ocaml"; "link"; "use_ppx_jsobject_conv"; "byte"] & *)
     (*   A("./src/ppx_jsobject_conv_runtime.cma"); *)
     (* flag ["ocaml"; "link"; "use_ppx_jsobject_conv"; "native"] & *)
     (*   A("./src/ppx_jsobject_conv_runtime.cmxa"); *)
     ocaml_lib ~dir:"src" "src/ppx_jsobject_conv_runtime";
     (* Pass -predicates to ocamldep *)
     pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])

  | _ -> ())
