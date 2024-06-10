open Ppxlib

module Ast = Ast_builder.Default

(** Build the expresssion [Modpath.s] *)
let suffix_lid modpath s =
  Ast.pexp_ident ~loc:modpath.loc @@
  Ast.Located.map (fun lid -> Ldot (lid, s)) modpath

(** Apply {!Ppx_tmaa_runtime.Destiny.fill} *)
let destiny_fill ~loc dest e =
  [%expr Ppx_tmaa_runtime.Destiny.fill [%e dest] [%e e]]

(** make a fresh destiny variable *)
let fresh_dest loc = 
  Ast.Located.mk ~loc (gen_symbol ~prefix:"dest" ())

let with_new_dest expr f =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let dest = fresh_dest loc in
  let dest_lid = Ast.(pexp_ident ~loc @@ Located.map_lident dest) in
  let vb = Ast.(value_binding ~loc ~pat:(ppat_var ~loc dest) ~expr) in
  Ast.(pexp_let ~loc Nonrecursive [vb] @@ f dest_lid)

(** Transformation to destiny passing style.

    Take as argument the module path pointing to a Promise module. *)
let rewrite_dps modpath =
  let await_fun = suffix_lid modpath "await" in
  object (self)
    (* Context is the destiny of the computation *)
    inherit [_] Ast_traverse.map_with_context as _super

    (** Some functions from [Modpath] *)

    method! expression dest e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      (* Context *)
      | Pexp_constraint (body, ty) ->
        let ty = [%type: [%t ty] Ppx_tmaa_runtime.Destiny.t] in
        with_new_dest
          { e with pexp_desc = Pexp_constraint (dest, ty) }
          (fun dest' ->
             self#expression dest' body)          
      | Pexp_coerce (body, ty1, ty2) ->
        let ty1 =
          Option.map
            (fun ty1 -> [%type: [%t ty1] Ppx_tmaa_runtime.Destiny.t])
            ty1
        in
        let ty2 = [%type: [%t ty2] Ppx_tmaa_runtime.Destiny.t] in
        with_new_dest
          { e with pexp_desc = Pexp_coerce (dest, ty1, ty2) }
          (fun dest' ->
             self#expression dest' body)       
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
      | Pexp_assert _
        -> destiny_fill ~loc:e.pexp_loc dest e

      (* Others/Impossible to know *) 
      | Pexp_letop _
      | Pexp_extension _
      | Pexp_unreachable
        -> destiny_fill ~loc:e.pexp_loc dest e
  end

let rec wrap_function_args rev_arg_list f e = match e.pexp_desc with
  | Pexp_newtype (_ty, body) ->
    wrap_function_args rev_arg_list f body
  | Pexp_fun (lbl, _opt, _pat, body) ->
    let pat_loc = _pat.ppat_loc in
    let name = gen_symbol ~prefix:"arg$" () in
    let pat = Ast.ppat_var ~loc:pat_loc @@ Ast.Located.mk ~loc:pat_loc name in
    let arg = Ast.pexp_ident ~loc:pat_loc @@ Ast.Located.lident ~loc:pat_loc name in
    { e with pexp_desc =
               Pexp_fun (lbl, None, pat,
                         wrap_function_args ((lbl,arg)::rev_arg_list) f body) }
  (* | Pexp_function cases -> *)
  (*   let arg =  *)
  (*   let cases = *)
  (*     List.map *)
  (*       (fun case -> {case with pc_rhs = map_function_body (n+1) f case.pc_rhs}) *)
  (*       cases *)
  (*   in *)
  (*   { e with pexp_desc = Pexp_function cases } *)
  | _ ->
    (* Found the body *)
    f (List.rev rev_arg_list) e
let wrap_function_args = wrap_function_args []

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

let dps_function_body o e =
  let loc = e.pexp_loc in
  let dest = fresh_dest loc in
  let dest_expr = Ast.(pexp_ident ~loc @@ Located.map_lident dest) in
  Ast.pexp_fun ~loc Nolabel None (Ast.ppat_var ~loc dest) @@
  map_function_body (fun e -> o#expression dest_expr e) e

let dps_function_name =
  Ast.Located.map (fun orig -> orig ^ "$dps")

let entry_function_body dps_fun e =
  wrap_function_args
    (fun args body ->
       let loc = body.pexp_loc in
       let destiny = [%expr d] in
       let dps_call = Ast.pexp_apply ~loc dps_fun ((Nolabel, destiny)::args) in
       [%expr 
         Eio.Switch.run @@ fun sw ->
         let p, resolver = Eio.Promise.create () in
         let d = Ppx_tmaa_runtime.Destiny.create (Eio.Promise.resolve resolver) in
         Eio.Fiber.fork ~sw (fun () -> [%e dps_call]);
         p
       ]
    )
    e

let rewrite_async_bindings
    ~(modpath : longident loc) ~loc (r : rec_flag) (l : value_binding list) =
  let o = rewrite_dps modpath in
  let f { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
    let dps_fun = 
      let loc = pvb_pat.ppat_loc in
      match pvb_pat.ppat_desc with
      | Ppat_var name -> dps_function_name name
      | _ -> Ast.Located.mk ~loc (gen_symbol ~prefix:"$dps" ())
    in
    let vb =
      let dps_fun_expr =
        Ast.pexp_ident ~loc:pvb_loc @@ Ast.Located.map_lident dps_fun
      in
      let pvb_expr = entry_function_body dps_fun_expr pvb_expr in
      { pvb_pat ; pvb_attributes; pvb_loc; pvb_expr }
    and vb_dps =
      let pvb_pat = Ast.ppat_var ~loc:pvb_pat.ppat_loc dps_fun in
      let pvb_expr = dps_function_body o pvb_expr in
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
    Option.value ~default:(Ast_builder.Default.Located.lident ~loc "Eio.Promise")
      arg
  in
  List.concat_map
    (fun (loc, rec_flag, l) -> rewrite_async_bindings ~modpath ~loc rec_flag l)
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
