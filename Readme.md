
# ppx_jsobject_conv

*Work in progress!*

Ppx plugin for `Typeconv` to derive conversion from ocaml types to js objects to use with `js_of_ocaml`.

*Will be published to opam soon*

## Example usage

See [expample of exported ocaml module](https://github.com/little-arhat/exported-ocaml-js) for more info.

Short guide:

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

To overcome this issue, you will have to use different set of packages for compiling and linking: for compile phase you only need `ppx_jsobject_conv`, that
will rewrite source code to include conversion routines. For linking, you don't need `ppx_jsobject_conv` with all that stuff `ppx_type_conv` brings on board.
You will only need to add `ppx_jsobject_conv.runtime` that contains all necessary functions, used by generated code. To achieve this, add something like this
to your `_tags`:

```
"src/core.byte": package(ppx_jsobject_conv.runtime)
<src/*.ml{,i,y}>: package(ppx_jsobject_conv)
```

If you're using `_oasis`, the best way is to not list `ppx_jsobject_conv` in `BuildDepends` at all, and just add two lines above after `# OASIS STOP`. Note
that oasis will not check for library in that case, but I don't know better way to do this, yet %)

And, btw, I think smth like this should be done by default -- using `ppx_type_conv`, or any of its plugins, should not incur such penalty, and libraris, acting
as shell for ppx rewriters should only be used at compile phase.

## Standalone rewriter

Package also installs ppx-driver `ppx_jsobject.byte` that is useful for testing.

## Build

`make`
