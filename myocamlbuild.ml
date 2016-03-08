open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    let ppx_driver_component deriver =
      (Findlib.query "ppx_driver").Findlib.location ^ "/" ^ deriver
    in
    flag ["ocaml"; "compile"; "use_js_of_ocaml"] &
      S[A"-ppx"; A("ocamlfind ppx_driver/ppx_type_cnov "^
                     "src/ppx_jsobject_conv.cma ");
        A"-I"; A(ppx_driver_component "")];
    (* flag ["ocaml"; "link"; "use_js_of_ocaml"; "native"] & *)
    (*   A(ppx_driver_component "ppx_jsobject_conv_runtime.cmxa"); *)
    ocaml_lib ~dir:"src" "src/ppx_jsobject_conv_runtime";
    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])

  | _ -> ())
