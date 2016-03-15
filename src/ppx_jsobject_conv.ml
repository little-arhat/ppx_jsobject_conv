open StdLabels

open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default
module Type_conv = Ppx_type_conv.Std.Type_conv

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

let wrap_runtime decls =
    [%expr let open! Ppx_jsobject_conv_runtime in [%e decls]]

let input_evar ~loc= evar ~loc "v"
let input_pvar ~loc= pvar ~loc "v"

module Attrs = struct
  let name =
    Attribute.declare "jsobject.name"
                      Attribute.Context.constructor_declaration
                      (Ast_pattern.single_expr_payload
                         (Ast_pattern.estring Ast_pattern.__))
                      (fun x -> x)

  let constructor_name cd  =
    match Attribute.get name cd with
    | Some(v) -> v
    | None -> cd.pcd_name.txt

  let key =
    Attribute.declare "jsobject.key"
                      Attribute.Context.label_declaration
                      (Ast_pattern.single_expr_payload
                         (Ast_pattern.estring Ast_pattern.__))
                      (fun x -> x)
  let field_name ld =
    match Attribute.get key ld with
    | Some(v) -> v
    | None -> ld.pld_name.txt

end

(* Courtesy of ppx_sexp_conv *)
module Fun_or_match = struct
  type t =
    | Fun   of expression
    | Match of case list

  let expr ~loc t =
    match t with
    | Fun f       -> f
    | Match cases -> pexp_function ~loc cases

  let unroll ~loc e t =
    match t with
    | Fun f       -> eapply ~loc f [e]
    | Match cases -> pexp_match ~loc e cases

  (** For list of expressions returns triple (bindings, pvars, evars)
      where bindings is list of `let v_i =`;
            pvars is list of `v_i` for patterns, and
            evars is list of expressions applied to corresponding temp var
   *)
  let map_tmp_vars ~loc ts =
    let vars = List.mapi ts ~f:(fun i _ -> "v" ^ string_of_int i) in
    let bindings =
      List.map2 vars ts ~f:(fun var t ->
        let expr = unroll ~loc (evar ~loc var) t in
        value_binding ~loc ~pat:(pvar ~loc var) ~expr)
    in
    (bindings,
     List.map vars ~f:(pvar ~loc),
     List.map vars ~f:(evar ~loc))
end

let jsobject_type_is_recursive =
  types_are_recursive ~short_circuit:(fun _typ -> None)

let really_recursive rec_flag tds =
  match rec_flag with
  | Recursive    -> if jsobject_type_is_recursive tds then Recursive else Nonrecursive
  | Nonrecursive -> Nonrecursive

let constrained_function_binding = fun
  (* placing a suitably polymorphic or rigid type constraint on the pattern or body *)
  ~(loc:Location.t) ~(func_name:string) (body:expression) ->
  let pat = pvar ~loc func_name in
  value_binding ~loc ~pat ~expr:body

module Jsobject_of_expander = struct
  let mk_type td =
    combinator_type_of_type_declaration
      td ~f:(fun ~loc:_ ty ->
        [%type: [%t ty] -> Js.Unsafe.any])

  let name_of_td td = match td.ptype_name.txt with
    | "t" -> "jsobject_of"
    | tn  -> "jsobject_of_" ^ tn

  let jsobject_of_std_type (id : Longident.t Located.t) =
    let txt : Longident.t =
      match id.txt with
      | Longident.Lident   s  -> Longident.Lident  ("jsobject_of_" ^ s)
      | Longident.Ldot (p, s) -> Longident.Ldot (p, "jsobject_of_" ^ s)
      | Longident.Lapply _    -> failwith "ppx_jsobject_conv: jsobject_std_type"
    in
    pexp_ident ~loc:id.loc { id with txt }

  let rec jsobject_of_type (typ: core_type) : Fun_or_match.t =
    let loc = typ.ptyp_loc in
    match typ.ptyp_desc with
    | Ptyp_constr (cid, args) ->
       let init = jsobject_of_std_type cid in
       Fun_or_match.Fun (List.fold_right
              args
              ~init
              ~f:(fun tp2 exp1  ->
                let exp2 = Fun_or_match.expr ~loc (jsobject_of_type tp2) in
                [%expr [%e exp1] [%e exp2]]))
    | Ptyp_tuple tps ->
       Fun_or_match.Match [jsobject_of_tuple (loc, tps)]
    | Ptyp_variant (row_fields, _, _) ->
       jsobject_of_variant (loc, row_fields)
    | Ptyp_object(_) | Ptyp_class(_) ->
       Location.raise_errorf ~loc "ppx_jsobject_conv: jsobject_of_type -- classes & objects are not supported yet"
    | Ptyp_package(_) ->
       Location.raise_errorf ~loc "ppx_jsobject_conv: jsobject_of_type -- modules are not supported yet"
    | Ptyp_any | Ptyp_var(_) | Ptyp_arrow(_) | Ptyp_alias(_)
      | Ptyp_poly(_) | Ptyp_extension(_) ->
       Location.raise_errorf ~loc "ppx_jsobject_conv: jsobject_of_type -- Unsupported type"
  (* Conversion of tuples *)
  and jsobject_of_tuple (loc, tps) =
    let fps = List.map ~f:jsobject_of_type tps in
    let bindings, pvars, evars = Fun_or_match.map_tmp_vars ~loc fps in
    let in_expr = [%expr
                      to_js_array
                      [%e elist ~loc evars] ] in
    let expr = pexp_let ~loc Nonrecursive bindings in_expr in
    ppat_tuple ~loc pvars --> expr
  (* Conversion of variant types *)
  and jsobject_of_variant (loc, row_fields) =
    let item = function
      | Rtag (cnstr, _, true, []) ->
         ppat_variant ~loc cnstr None -->
           [%expr to_js_array [(jsobject_of_string [%e estring ~loc cnstr])]]
      | Rtag (cnstr, _, false, [tp]) ->
        let cnstr_expr = [%expr jsobject_of_string [%e estring ~loc cnstr] ] in
        let var, patt = evar ~loc "v0", pvar ~loc "v0" in
        let cnstr_arg = Fun_or_match.unroll ~loc var (jsobject_of_type tp) in
        let expr = [%expr to_js_array [%e elist ~loc [cnstr_expr; cnstr_arg]]] in
        ppat_variant ~loc cnstr (Some patt) --> expr
      | Rtag (_) | Rinherit(_) ->
         Location.raise_errorf ~loc "ppx_jsobject_conv: unsupported jsobject_of_variant"
    in Fun_or_match.Match (List.map ~f:item row_fields)

  (* Conversion of sum types *)
  let jsobject_of_sum cds =
    let item cd =
      let loc = cd.pcd_loc in
      let lid = Located.map lident cd.pcd_name in
      let scnstr = estring ~loc (Attrs.constructor_name cd) in
      match cd.pcd_args with
      | [] ->
         ppat_construct ~loc lid None -->
           [%expr to_js_array [jsobject_of_string [%e scnstr]]]
      | args ->
         let jsobject_of_args = List.map ~f:jsobject_of_type args in
         let cnstr_expr = [%expr (jsobject_of_string [%e scnstr])] in
         let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc jsobject_of_args in
         let patt =
           match patts with
           | [patt] -> patt
           | _ -> ppat_tuple ~loc patts
         in
         ppat_construct ~loc lid (Some patt) -->
           pexp_let ~loc
                    Nonrecursive
                    bindings
                    [%expr to_js_array
                           [%e elist ~loc (cnstr_expr :: vars)]]
    in
    Fun_or_match.Match (List.map ~f:item cds)

  (* Conversion of record types *)
  let mk_rec_patt loc patt name =
    let p =
      Location.mkloc (Longident.Lident name) loc ,
      pvar ~loc ("v_" ^ name)
    in
    patt @ [p]

  let jsobject_of_record ~loc fields =
    let item (patts, exprs) = function
      | {pld_name = {txt=name; loc}; pld_type = tp; _ } as ld ->
          let patts = mk_rec_patt loc patts name in
          let vname = evar ~loc ("v_" ^ name) in
          let field_name = estring ~loc (Attrs.field_name ld) in
          let cnv = Fun_or_match.unroll ~loc vname (jsobject_of_type tp) in
          let expr =
            [%expr
                ([%e field_name], [%e cnv])]
          in
          patts, expr::exprs
    in
    let patts, exprs = List.fold_left ~f:item ~init:([], []) fields in
    let expr = Ast_helper.Exp.array ~loc (List.rev exprs) in
    Fun_or_match.Match [
        ppat_record ~loc patts Closed -->
          [%expr make_jsobject [%e expr]]
      ]

  let jsobject_of_td td =
    let tps = List.map td.ptype_params ~f:(fun tp -> (get_type_param_name tp).txt) in
    (* TODO: pass type_name further to display nice errors *)
    let {ptype_name = {txt = _type_name; loc = _}; ptype_loc = loc; _} = td in
    let body =
      match td.ptype_kind with
      | Ptype_abstract -> begin
          match td.ptype_manifest with
          | Some ty -> jsobject_of_type ty
          | None -> Location.raise_errorf ~loc "ppx_jsobject_conv: fully abstract types are not supported"
        end
      | Ptype_variant cds -> jsobject_of_sum cds
      | Ptype_record fields -> jsobject_of_record ~loc fields
      | Ptype_open -> Location.raise_errorf ~loc "ppx_jsobject_conv: open types are not supported"
    in
    let body' = match body with
      | Fun_or_match.Fun fun_expr -> [%expr fun [%p input_pvar ~loc] ->
                                            [%e fun_expr] [%e input_evar ~loc]]
      | Fun_or_match.Match matchings -> pexp_function ~loc matchings
    in
    let func_name = name_of_td td in
    let body'' =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_of_" ^ id)) in
      eabstract ~loc patts @@ wrap_runtime body'
    in
    [constrained_function_binding ~loc ~func_name body'']


  let str_type_decl ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let bindings = List.map tds ~f:jsobject_of_td |> List.concat in
    [pstr_value ~loc rec_flag bindings]

  let sig_type_decl ~loc:_ ~path:_ (_rf, tds) =
    List.map tds
             ~f:(fun td ->
               let jsobject_of = mk_type td in
               let name = name_of_td td in
               let loc = td.ptype_loc in
               psig_value ~loc (value_description
                                  ~loc
                                  ~name:{ td.ptype_name with txt = name }
                                  ~type_:jsobject_of ~prim:[]))
  let core_type ty =
    jsobject_of_type ty |> Fun_or_match.expr ~loc:ty.ptyp_loc

end

module Jsobject_of = struct
  let str_type_decl =
    Type_conv.Generator.make_noarg
      Jsobject_of_expander.str_type_decl
      ~attributes:[Attribute.T Attrs.name;
                   Attribute.T Attrs.key;
                  ]
  ;;

  let sig_type_decl =
    Type_conv.Generator.make_noarg Jsobject_of_expander.sig_type_decl
  ;;

  let extension ~loc:_ ~path:_ ctyp = Jsobject_of_expander.core_type ctyp

  let deriver =
    Type_conv.add "jsobject_of"
      ~str_type_decl
      ~sig_type_decl
      ~extension
  ;;
end

module Of_jsobject_expander = struct
  let mk_type td =
    combinator_type_of_type_declaration
      td ~f:(fun ~loc:_ ty ->
        [%type: Js.Unsafe.any -> ([%t ty], string) Result.t ])

  let name_of_td td = match td.ptype_name.txt with
    | "t" -> "of_jsobject_res"
    | tn  -> tn ^ "_of_jsobject_res"

  let eok ~loc v = pexp_construct
                     ~loc (Located.lident ~loc "Result.Ok") (Some v)
  let err_simple ~loc s = pexp_construct
                            ~loc (Located.lident ~loc "Result.Error")
                            (Some (estring ~loc s))
  let err_var ~loc s var =
    let base = estring ~loc s in
    let full = [%expr [%e base] ^ [%e var]] in
    pexp_construct ~loc (Located.lident ~loc "Result.Error") (Some full)

  let std_type_of_jsobject_res (id : Longident.t Located.t) =
    let txt : Longident.t =
      match id.txt with
      | Longident.Lident   s  -> Longident.Lident  (s ^ "_of_jsobject_res")
      | Longident.Ldot (p, s) -> Longident.Ldot (p, s ^ "_of_jsobject_res")
      | Longident.Lapply _    -> failwith "ppx_jsobject_conv: type_id_of_jsobject"
    in
    pexp_ident ~loc:id.loc { id with txt }

  let rec type_of_jsobject (type_name: string) (typ: core_type) : Fun_or_match.t =
    let loc = typ.ptyp_loc in
    match typ.ptyp_desc with
    | Ptyp_constr (id, args) ->
      let init = std_type_of_jsobject_res id in
      let args = List.map args
                          ~f:(fun arg ->
                            Fun_or_match.expr ~loc
                                              (type_of_jsobject type_name arg)) in
      Fun_or_match.Fun (eapply ~loc init args)
    | Ptyp_tuple tps -> tuple_of_jsobject ~loc type_name tps
    | Ptyp_variant(row_fields, _, _) ->
       variant_of_jsobject ~loc type_name row_fields
    | Ptyp_object(_) | Ptyp_class(_) ->
       Location.raise_errorf ~loc "ppx_jsobject_conv: type_of_jsobject -- classes & objects are not supported yet"
    | Ptyp_package(_) ->
       Location.raise_errorf ~loc "ppx_jsobject_conv: type_of_jsobject -- modules are not supported yet"
    | Ptyp_any | Ptyp_var(_) | Ptyp_arrow(_) | Ptyp_alias(_)
      | Ptyp_poly(_) | Ptyp_extension(_) ->
       Location.raise_errorf ~loc "ppx_jsobject_conv: type_of_jsobject -- Unsupported type"
  and tuple_of_jsobject ~loc type_name tps =
    let fps = List.map ~f:(type_of_jsobject type_name) tps in
    let efps = List.map ~f:(Fun_or_match.expr ~loc) fps in
    let _, pvars, evars = Fun_or_match.map_tmp_vars ~loc fps in
    let iefps = List.mapi ~f:(fun i fp -> (i, fp)) efps in
    let inner_expr = eok ~loc (pexp_tuple ~loc evars) in
    let earr, parr = evar ~loc "arr", pvar ~loc "arr" in
    let body = List.fold_right2
                 ~init: inner_expr
                 ~f:(fun pvar (i, fp) acc ->
                   let ei = eint ~loc i in
                   [%expr
                       array_get_or_error [%e earr] [%e ei]
                       >>= [%e fp]
                       >>= (fun [%p pvar ] ->
                                [%e acc])])
                 pvars iefps
    in
    let num = List.length fps in
    let outer_expr = [%expr
                         (fun t ->
                           is_array_of_size_n
                             t [%e eint ~loc num]
                           >>= (fun [%p parr] -> [%e body]))
                     ] in
    Fun_or_match.Fun outer_expr

  and variant_of_jsobject ~loc type_name row_fields =
    let earr, parr = evar ~loc "arr", pvar ~loc "arr" in
    let item = function
      (* p. variant constructor w/o arguments*)
      | Rtag (cnstr, _, true , []) ->
         let ecnstr = Ast_helper.Exp.variant ~loc cnstr None in
         pstring ~loc cnstr -->
           [%expr [%e eok ~loc ecnstr]]
      (* p. variant constructor w argument *)
      | Rtag (cnstr, _, false, [tp]) ->
         let ev, pv = evar ~loc "v0", pvar ~loc "v0" in
         let cnstr_fun =
           Fun_or_match.expr ~loc (type_of_jsobject type_name tp) in
         let ecnstr = Ast_helper.Exp.variant ~loc cnstr (Some ev) in
         pstring ~loc cnstr -->
           [%expr
               array_get_or_error [%e earr] 1
               >>= [%e cnstr_fun]
               >>= (fun [%p pv] ->
                    [%e eok ~loc ecnstr]
               )]
      | Rtag(_) | Rinherit(_) ->
         Location.raise_errorf "ppx_jsobject_conv: unsupported variant_of_jsobject"
    in
    let matches = List.map ~f:item row_fields in
    let empty_match = [pvar ~loc "unknown" -->
                         err_var ~loc
                                 ("Unknown constructor for type " ^ type_name ^ " : ")
                                 (evar ~loc "unknown")] in
    let match_expr = Fun_or_match.expr ~loc @@
                 Fun_or_match.Match (matches @ empty_match) in
    let outer_expr = [%expr
                         (fun pv ->
                           is_array pv >>=
                             (fun [%p parr] ->
                               array_get_or_error [%e earr] 0
                               >>= string_of_jsobject_res
                               >>= [%e match_expr]))]
    in Fun_or_match.Fun outer_expr

  let sum_of_jsobject ~loc type_name cds =
    let earr, parr = evar ~loc "arr", pvar ~loc "arr" in
    let item cd =
      let pcnstr = pstring ~loc (Attrs.constructor_name cd) in
      match cd.pcd_args with
      | [] ->
         pcnstr -->
           [%expr [%e eok ~loc (econstruct cd None)]]
      | args ->
         let fargs = List.map ~f:(type_of_jsobject type_name) args in
         let efargs = List.map ~f:(Fun_or_match.expr ~loc) fargs in
         let _, pvars, evars = Fun_or_match.map_tmp_vars ~loc fargs in
         (* first element is constructor name *)
         let iefargs = List.mapi ~f:(fun i fp -> (i + 1, fp)) efargs in
         let econstr = econstruct cd (Some (pexp_tuple ~loc evars)) in
         let inner_expr = eok ~loc econstr in
         (* (fun [%p input_pvar ~loc ] -> [%e fa] v)
            not very good, maybe generate randomized "input_vars"
            and pass them from the top?
          *)
         let body = List.fold_right2
                      ~init: inner_expr
                      ~f:(fun pvar (i, fa) acc ->
                        let ei = eint ~loc i in
                        [%expr
                            array_get_or_error [%e earr] [%e ei]
                         >>= [%e fa]
                         >>= (fun [%p pvar ] ->
                           [%e acc])])
                      pvars iefargs
         in
         pcnstr --> body
    in
    let matches = List.map ~f:item cds in
    let empty_match = [pvar ~loc "unknown" -->
                         err_var ~loc
                                 ("Unknown constructor for type " ^ type_name ^ " : ")
                                 (evar ~loc "unknown")] in
    let match_expr  = Fun_or_match.expr ~loc @@
                        Fun_or_match.Match (matches @ empty_match) in
    let outer_expr = [%expr
                         (fun s ->
                           is_array [%e input_evar ~loc] >>=
                             (fun [%p parr] ->
                               array_get_or_error [%e earr] 0
                               >>= string_of_jsobject_res
                               >>= [%e match_expr]))]
    in Fun_or_match.Fun outer_expr

  let mk_rec_details type_name = function
    | {pld_name = {txt=name; loc}; pld_type = tp; _ } as ld ->
       let vname = evar ~loc ("v_" ^ name) in
       let field_name = estring ~loc (Attrs.field_name ld) in
       let pname = pvar ~loc ("v_" ^ name) in
       let cnv = Fun_or_match.expr
                   ~loc (type_of_jsobject type_name tp) in
       let lid = Located.lident ~loc name in
       ((lid, vname), (pname, field_name, cnv))

  let record_of_jsobject ~loc type_name fields =
    let rec_details = List.map ~f:(mk_rec_details type_name) fields in
    let lidexprs, pfc = List.split rec_details in
    let inner_expr = eok ~loc (Ast_helper.Exp.record ~loc lidexprs None) in
    let eobj, pobj = evar ~loc "obj", pvar ~loc "obj" in
    let body = List.fold_right
                 ~init: inner_expr
                 ~f:(fun (pvar, field_name, cnv) acc ->
                   [%expr
                       object_get_key [%e eobj] [%e field_name]
                       >>= [%e cnv]
                       >>= (fun [%p pvar] ->
                         [%e acc])]
                 ) pfc
    in
    let outer_expr = [%expr
                         (fun r ->
                           is_object [%e input_evar ~loc] >>=
                             (fun [%p pobj] ->
                               [%e body]))] in
    Fun_or_match.Fun outer_expr


  let td_of_jsobject td =
    let tps = List.map td.ptype_params ~f:(fun tp -> (get_type_param_name tp).txt) in
    let {ptype_name = {txt = type_name; loc = _}; ptype_loc = loc; _} = td in
    let is_private = (match td.ptype_private with Private -> true | Public -> false) in
    if is_private
    then Location.raise_errorf ~loc "of_sexp is not supported for private type";
    let body =
      match td.ptype_kind with
      | Ptype_abstract -> begin
          match td.ptype_manifest with
          | Some ty -> type_of_jsobject type_name ty
          | _ -> Location.raise_errorf ~loc "ppx_jsobject_conv: fully abstract types are not supported"
        end
      | Ptype_variant cds -> sum_of_jsobject ~loc type_name cds
      | Ptype_record fields -> record_of_jsobject ~loc type_name fields
      | Ptype_open -> Location.raise_errorf ~loc "ppx_jsobject_conv: open types are not supported"
    in
    let body' = match body with
      | Fun_or_match.Fun fun_expr -> [%expr fun [%p input_pvar ~loc] ->
                                            [%e fun_expr] [%e input_evar ~loc]]
      | Fun_or_match.Match matchings -> pexp_function ~loc matchings
    in
    let func_name = name_of_td td in
    let body'' =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_of_" ^ id)) in
      eabstract ~loc patts @@ wrap_runtime body'
    in
    [constrained_function_binding ~loc ~func_name body'']


  let str_type_decl ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let bindings = List.map tds ~f:td_of_jsobject |> List.concat in
    [pstr_value ~loc rec_flag bindings]


  let sig_type_decl ~loc:_ ~path:_ (_rf, tds) =
    List.map tds
             ~f:(fun td ->
               let of_jsobject = mk_type td in
               let name = name_of_td td in
               let loc = td.ptype_loc in
               psig_value ~loc (value_description
                                  ~loc
                                  ~name:{ td.ptype_name with txt = name }
                                  ~type_:of_jsobject ~prim:[]))
  let core_type ty =
    (* XXX: empty type_name? how is core_type use*)
    type_of_jsobject "" ty |> Fun_or_match.expr ~loc:ty.ptyp_loc

  end

module Of_jsobject = struct
  let str_type_decl =
    Type_conv.Generator.make_noarg
      Of_jsobject_expander.str_type_decl
      ~attributes:[Attribute.T Attrs.name;
                   Attribute.T Attrs.key;
                  ]
  ;;

  let sig_type_decl =
    Type_conv.Generator.make_noarg Of_jsobject_expander.sig_type_decl
  ;;

  let extension ~loc:_ ~path:_ ctyp = Of_jsobject_expander.core_type ctyp

  let deriver =
    Type_conv.add "of_jsobject"
      ~str_type_decl
      ~sig_type_decl
      ~extension
  ;;
end


let () =
  Type_conv.add_alias "jsobject"
                      [Jsobject_of.deriver ;
                       Of_jsobject.deriver
                      ]
                      ~sig_exception:[Jsobject_of.deriver]
                      ~str_exception:[Jsobject_of.deriver]
  |> Type_conv.ignore;;
