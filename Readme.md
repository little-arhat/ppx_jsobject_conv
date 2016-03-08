
# ppx_jsobject_conv

*Work in progress!*

Ppx plugin for `Typeconv` to derive conversion from ocaml types to js objects to use with `js_of_ocaml`.

*Will be published to opam soon*

## Example usage

```ocaml

type stuff = int * string * float [@@deriving jsobject]

type status = Created | Registered of int | Deleted of stuff [@@deriving jsobject]

type user = {
    name: string;
    age: int;
    status: status
} [@@deriving jsobject]

```

This will generate functions `jsobject_of_stuff`, `jsobject_of_status`, `jsobject_of_user`. Conversions from jsobjects to OCaml values will be added later.

### Adding ppx_jsobject_conv to your project

Easiest way to use `[@@deriving jsobject]` is to link againts `ppx_jsobject_conv` (`ppx_jsobject_conv`
in `BuildDepents` for `_oasis` or `package(ppx_jsobject_conv)` in `_tags` if you use `ocamlbuild` directly). This will add all necessary dependencies
and will rewrite your source files. This, however, currently leads to inclusion of full `ppx_type_conv`, `ppx_core` and other stuff, so `js_of_ocaml` produces
HUGE js file. You will also have to add `+weak.js` and `+toplevel.js`, since some functions from there are referenced by `ppx_type_conv`.

To overcome this issue, `ppx_jsobject_conv` provides binary `ppx_jsobject.byte` that implements `ppx_driver` you can use separately from your program.
Add `ByteOpt+:       -ppx "ppx_jsobject.byte -as-ppx"` to your `_oasis` file, to process your source file with ppx extension. Note, you will have to add
`ppx_jsobject_conv.runtime` as `BuildDepends` manually.

## Build

`make`
