#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "ppx-jsobject-conv-tests" ~builder:`OCamlbuild [
                 Pkg.bin "src_test/test.byte";
               ]
