#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "ppx_jsobject_conv" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "src/ppx_jsobject_conv";
    Pkg.lib ~exts:Exts.module_library "src/ppx_jsobject_conv_runtime";]
