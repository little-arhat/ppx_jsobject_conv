# Ppx_jsobject_conv

Ppx plugin for `Typeconv` to derive conversion from ocaml types to js objects to use with `js_of_ocaml`.

For types annotated with `[@@deriving jsobject]`, plugin will generate pair of functions: `*_of_jsobject/jsobject_of_*`
to convert from/to JavaScript objects. This allows one to use clean OCaml types to describe their logic, while having ability
to easy go down to js types. Easy conversion from js objects to OCaml types means also, one can use fast native `JSON.parse` to
convert JSON to OCaml types.

MIT License.

## Example usage

See [expample of exported ocaml module](https://github.com/little-arhat/exported-ocaml-js) or `src_test` for more info.

### Short guide

```ocaml

module Stuff = struct
  type t = int * string * float [@@deriving jsobject]
end

type status = Created | Registered of int | Deleted of Stuff.t [@@deriving jsobject]

type user = {
    name: string;
    age: int;
    status: status
} [@@deriving jsobject]

```

This will generate functions `Stuff.of_jsobject/Stuff.jsobject_of`, `statls_of_jsobject/jsobject_of_status`, `user_of_jsobject/jsobject_of_user`.

### More details

Given `type t = ... [@@deriving jsobject]`, signatures for functions will be:

```ocaml
val jsobject_of : t -> 'a Js.t
val of_jsobject : 'a Js.t -> (t, string) Result.result
```

One can derive only one function from the pair using `[@@deriving jsobject_of]` or `[@@deriving of_jsobject]`, and define complementary function manually,
following signatures above.

For types named `t` generated functions have names without any prefixes, otherwise `<typename_of_jsobject/jsobject_of_<typename>` used.

### Error reporting

If conversion from JS type to OCaml was unsuccessful, `Error` constructor of `Result.result` type will be returned with the description of error.
If error happened somewhere deep in the structure, error description will have prefix with path to errorneus field: `message.1.query.condition.0`.

## Adding ppx_jsobject_conv to your project

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
