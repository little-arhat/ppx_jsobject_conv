
type 'a dummy_bool_expr = Dor of 'a list option [@jsobject.name "$or"] [@jsobject.sum_type_as "object"]
                        | Dand of 'a list option [@jsobject.name "$and"]
                                     [@@deriving jsobject]

type 'a cond_expr = 'a [@@deriving jsobject_of]

type 'a cond_expr = 'a [@@deriving jsobject_of]
let rec cond_expr_of_jsobject _of_a obj =
  match dummy_bool_expr_of_jsobject (cond_expr_of_jsobject _of_a) obj with
  | Ok dbe ->
     (match dbe with
      | Dor (Some [x]) -> Ok x
      | Dand (Some [x]) -> Ok x
      | Dor (None) -> Error "empty value for condition"
      | Dand (None) -> Error "empty value for condition"
      | Dor (_) -> Error "multiple values for condition"
      | Dand (_) -> Error "multiple values for condition")
  | Error _ -> _of_a obj

type bool_val = bool [@@deriving jsobject_of]
type cond =
  | ViewsCount of bool_val cond_expr [@jsobject.name "views_count"]
                           [@jsobject.sum_type_as "object"]
                           [@@deriving jsobject]
