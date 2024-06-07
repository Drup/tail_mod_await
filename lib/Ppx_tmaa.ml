open Ppxlib

module Ast = Ast_builder.Default

(** Build the expresssion [Modpath.s] *)
let suffix_lid modpath s =
  Ast.pexp_ident ~loc:modpath.loc @@
  Ast.Located.map (fun lid -> Ldot (lid, s)) modpath

(** Apply {!Ppx_tmaa_runtime.Destiny.fill} *)
let destiny_fill ~loc dest e =
  let d = Ast.pexp_ident ~loc @@ Ast.Located.map_lident dest in
  [%expr Ppx_tmaa_runtime.Destiny.fill [%e d] [%e e]]

(** make a fresh destiny variable *)
let mk_dest_variable loc =
  Ast.Located.mk ~loc (gen_symbol ~prefix:"$dest" ())

(** Transformation to destiny passing style.

    Take as argument the module path pointing to a Promise module. *)
let dps_transform modpath =
  let await_fun = suffix_lid modpath "await" in
  object (self)
    (* Context is the destiny of the computation *)
    inherit [_] Ast_traverse.map_with_context as _super

    (** Some functions from [Modpath] *)

    method! expression dest e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      (* Context *)
      | Pexp_constraint (_, _)
      | Pexp_coerce (_, _, _)
      | Pexp_tuple _
      | Pexp_construct (_, _)
      | Pexp_variant (_, _)
      | Pexp_record (_, _)
      | Pexp_array _
        (* TODO Actually handle the context *)
        -> destiny_fill ~loc:e.pexp_loc dest e

      (* Await context *)
      | Pexp_apply ({pexp_desc = Pexp_ident lid; _}, [Nolabel, e])
        when lid.txt = Ldot (modpath.txt, "await") ->
        (* TODO: Handle [await (f x)] *)
        (* TODO: Lift expressions in await *)
        (* TODO: Actually use chain *)
        destiny_fill ~loc dest
          [%expr [%e await_fun] [%e e]]      

      (* Return position *)
      | Pexp_ident _
      | Pexp_constant _
      | Pexp_function _
      | Pexp_fun (_, _, _, _)
      | Pexp_apply (_, _)
      | Pexp_field (_, _)
      | Pexp_setfield (_, _, _)
      | Pexp_send (_, _) 
      | Pexp_object _
      | Pexp_new _
      | Pexp_override _
      | Pexp_setinstvar (_, _)
      | Pexp_pack _
      | Pexp_newtype (_, _)
      | Pexp_lazy _
        -> destiny_fill ~loc:e.pexp_loc dest e

      (* Control construct, we can go down *)
      | Pexp_match (e, c) ->
        { e with
          pexp_desc = Pexp_match (e, self#cases dest c)
        }
      | Pexp_ifthenelse (i, then_, else_) ->
        { e with
          pexp_desc =
            Pexp_ifthenelse (i, self#expression dest then_,
                             Option.map (self#expression dest) else_)
        }
      | Pexp_let (s, e1, e2) ->
        { e with
          pexp_desc = Pexp_let (s, e1, self#expression dest e2)
        }
      | Pexp_letmodule (s, m, e) ->
        { e with
          pexp_desc = Pexp_letmodule (s, m, self#expression dest e)
        }
      | Pexp_letexception (ec, e) ->
        { e with
          pexp_desc = Pexp_letexception (ec, self#expression dest e)
        }
      | Pexp_open (o, e) ->
        { e with
          pexp_desc = Pexp_open (o, self#expression dest e)
        }
      | Pexp_sequence (e1, e2) ->
        { e with
          pexp_desc = Pexp_sequence (e1, self#expression dest e2)
        }
      | Pexp_poly (e, ty) ->
        { e with
          pexp_desc = Pexp_poly (self#expression dest e, ty)
        }

      (* Wrapping construct, we give up *)
      | Pexp_try (_, _)
      | Pexp_while (_, _)
      | Pexp_for (_, _, _, _, _)
        -> destiny_fill ~loc:e.pexp_loc dest e

      (* Others/Impossible to know *) 
      | Pexp_assert _
      | Pexp_letop _
      | Pexp_extension _
      | Pexp_unreachable
        -> destiny_fill ~loc:e.pexp_loc dest e
  end
  
let dps_function_name =
  Ast.Located.map (fun orig -> orig ^ "$dps")

let rec map_function_body f e = match e.pexp_desc with
  | Pexp_newtype (ty, body) ->
    { e with pexp_desc = Pexp_newtype (ty, map_function_body f body) }
  | Pexp_fun (lbl, opt, pat, body) ->
    { e with pexp_desc = Pexp_fun (lbl, opt, pat, map_function_body f body) }
  | Pexp_function cases ->
    let cases =
      List.map
        (fun case -> {case with pc_rhs = map_function_body f case.pc_rhs})
        cases
    in
    { e with pexp_desc = Pexp_function cases }
  | _ ->
    (* Found the body *)
    f e

let dps_transformation
    ~(modpath : longident loc) ~loc (r : rec_flag) (l : value_binding list) =
  let o = dps_transform modpath in
  let f { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
    let vb =
      let pvb_expr = pvb_expr (* TODO non dps wrapper *) in
      { pvb_pat ; pvb_attributes; pvb_loc; pvb_expr }
    and vb_dps =
      let pvb_pat =
        let loc = pvb_pat.ppat_loc in
        Ast.ppat_var ~loc @@ match pvb_pat.ppat_desc with
        | Ppat_var name -> dps_function_name name
        | _ -> Ast.Located.mk ~loc (gen_symbol ~prefix:"$dps" ())
      in
      let loc = pvb_expr.pexp_loc in
      let dest = mk_dest_variable loc in
      let pvb_expr =
        Ast.pexp_fun ~loc Nolabel None (Ast.ppat_var ~loc dest) @@
        map_function_body (o#expression dest) pvb_expr
      in
      { pvb_pat ; pvb_attributes = []; pvb_loc; pvb_expr }
    in
    vb, vb_dps
  in
  match r with
  | Nonrecursive ->
    let orig_bindings, dps_bindings = List.split @@ List.map f l in
    [ Ast.pstr_value ~loc Nonrecursive dps_bindings ;
      Ast.pstr_value ~loc Nonrecursive orig_bindings
    ]    
  | Recursive ->
    let l =
      List.concat_map (fun vb -> let vb, vb_dps = f vb in [vb_dps; vb]) l
    in
    [ Ast.pstr_value ~loc Recursive l ]


let context = Extension.Context.structure_item
let extracter =
  Ast_pattern.(pstr @@ many @@ pack3 @@ pstr_loc __ @@ pstr_value __ __)

let expander ~ctxt ~arg bindings =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let modpath =
    Option.value ~default:(Ast_builder.Default.Located.lident ~loc "Promise")
      arg
  in
  List.concat_map
    (fun (loc, rec_flag, l) ->
       dps_transformation ~modpath ~loc rec_flag l)
    bindings

let ext = Extension.V3.declare_inline_with_path_arg
    "async"
    context
    extracter
    expander

let () =
  Driver.V2.register_transformation
    ~extensions:[ext]
    "tail_mod_await"
