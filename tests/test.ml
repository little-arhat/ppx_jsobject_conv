type x = int * string * int [@@deriving jsobject]
type a = A | B of string | C of x [@@deriving jsobject]
type d = D of (int * string) | E of (string * int) [@@deriving jsobject]


type maybe_int = int option [@@deriving jsobject]
type arr_float = float array [@@deriving jsobject]
type string_list = string list [@@deriving jsobject]

type status = [`Created | `Registered of int | `Deleted of int] [@@deriving jsobject]

type user = {
    age: int;
    name: string;
    status: status
  } [@@deriving jsobject]
