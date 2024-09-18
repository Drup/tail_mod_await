open Ppxlib

module Ast = Ast_builder.Default

module DpsFuns = Set.Make(String)

(** Apply {!Ppx_tmaa_runtime.Destiny.fill} *)
let fill ~loc dest e =
  [%expr Ppx_tmaa_runtime.Destiny.fill [%e dest] [%e e]]

(** Apply {!Ppx_tmaa_runtime.Destiny.forward} *)
let forward ~loc dest e =
  [%expr Ppx_tmaa_runtime.Destiny.forward [%e dest] [%e e]]

(** {!Ppx_tmaa_runtime.Hole.Field} *)
let field ~loc i = 
  [%expr Ppx_tmaa_runtime.Hole.Field ([%e Ast.eint ~loc i], Obj)]
  
(** Apply {!Ppx_tmaa_runtime.Hole.hole} *)
let hole ~loc hole =
  [%expr Ppx_tmaa_runtime.Hole.hole [%e hole]]

let gen_var ~loc ~prefix () =
  let symb = gen_symbol ~prefix () in
  let e = Ast.pexp_ident ~loc @@ Ast.Located.lident ~loc symb in
  let pat = Ast.ppat_var ~loc @@ Ast.Located.mk ~loc symb in
  pat, e

(** make a fresh destiny variable *)
let fresh_dest loc = 
  gen_var ~loc ~prefix:"dest" ()

let with_new_dest expr f =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let dest_pat, dest = fresh_dest loc in
  let vb = Ast.(value_binding ~loc ~pat:dest_pat ~expr) in
  Ast.(pexp_let ~loc Nonrecursive [vb] @@ f dest)

let dps_function_name =
  Ast.Located.map (fun orig -> orig ^ "$dps")

(** Transformation to destiny passing style.

    Take as argument the module path pointing to a Promise module. *)
let rewrite_dps modpath dps_funs =
  object (self)
    (* Context is the destiny of the computation *)
    inherit [_] Ast_traverse.map_with_context as _super

    method build_context ~loc dest l k = 
      let rec walk_subexprs i refinements holes subexprs postexpr = function
        | [] ->
          List.rev refinements, List.rev holes,
          List.rev subexprs, List.rev postexpr
        | e :: t ->
          let dest_pat, dest_expr = fresh_dest loc in
          match self#cons_context dest_expr e with
          | `Value e ->
            walk_subexprs
              (i+1) refinements holes (e::subexprs) postexpr t
          | `Hole expr ->
            let hole_pat, hole_expr = gen_var ~loc ~prefix:"hole" () in
            let vb_hole =
              let expr = field ~loc i in
              Ast.value_binding ~loc ~pat:hole_pat ~expr
            in
            let refinements' = (dest_pat, hole_expr) :: refinements in
            let holes' = vb_hole :: holes in
            let subexpr = hole ~loc hole_expr in
            walk_subexprs
              (i+1) refinements' holes' (subexpr::subexprs) (expr::postexpr) t
      in
      let refinements, holes, subexprs, postexprs =
        walk_subexprs 0 [] [] [] [] l
      in
      match holes with
      | [] -> `Value (k subexprs)
      | _ ->
        let ctx_pat, ctx_expr = gen_var ~loc ~prefix:"v" () in
        let value_binding =
          let pat = ctx_pat and expr = k subexprs in
          Ast.value_binding ~loc ~pat ~expr
        in
        let refinement_binding =
          let destinies, holes = List.split refinements in
          let pat = Ast.plist ~loc destinies
          and expr =
            [%expr Ppx_tmaa_runtime.Destiny.refine
                [%e dest] [%e ctx_expr] [%e Ast.elist ~loc holes]]
          in Ast.value_binding ~loc ~pat ~expr
        in
        let e =
          Ast.pexp_let ~loc Nonrecursive holes @@
          Ast.pexp_let ~loc Nonrecursive [value_binding] @@
          Ast.pexp_let ~loc Nonrecursive [refinement_binding] @@
          Ast.esequence ~loc postexprs
        in
        `Hole e
    
    method cons_context dest e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      (* Tuple (e, ..., e) *)     
      | Pexp_tuple l ->
        let k l = { e with pexp_desc = Pexp_tuple l } in
        self#build_context ~loc dest l k
      (* Unitary Constructor K *)
      | Pexp_construct (_, None) ->
        `Value e
      (* N-ary Constructor K (e, ... , e) *)
      | Pexp_construct
          (constructor, Some ({pexp_desc = Pexp_tuple l; _} as tup)) ->
        let k l =
          let tup = { tup with pexp_desc = Pexp_tuple l } in
          { e with pexp_desc = Pexp_construct (constructor, Some tup) }
        in
        self#build_context ~loc dest l k
      (* Constructor K e -- not a tuple *)
      | Pexp_construct (constructor, Some e) ->
        let k l = match l with
          | [e] -> { e with pexp_desc = Pexp_construct (constructor, Some e) }
          | _ -> assert false
        in
        self#build_context ~loc dest [e] k
      | Pexp_variant (_, _)
      | Pexp_record (_, _)
      | Pexp_array _
        (* TODO Actually handle the context *)
        -> assert false

      (* [await (f x)] context *)
      | Pexp_apply (
          {pexp_desc = Pexp_ident lid; _},
          [Nolabel,
           ({pexp_desc=Pexp_apply ({pexp_desc=Pexp_ident funlid; _} as funname,
                                   args); _}
            as application)])
        when lid.txt = Ldot (modpath.txt, "await")
             && DpsFuns.mem (Longident.name funlid.txt) dps_funs
        ->
        let new_fun_lid =
          Ast.Located.map_lident @@
          dps_function_name @@ Ast.Located.map Longident.name funlid
        in
        let dps_application = 
          {application with
           pexp_desc =
             Pexp_apply ({funname with pexp_desc=Pexp_ident new_fun_lid},
                         (Nolabel,dest)::args)}
        in
        `Hole
          [%expr Ppx_tmaa_runtime.AsyncFun.fork (fun () -> [%e dps_application])]
      (* [await e] context *)
      | Pexp_apply ({pexp_desc = Pexp_ident lid; _}, [Nolabel, body])
        when lid.txt = Ldot (modpath.txt, "await") ->
        `Hole (forward ~loc dest body)
      | _ ->
        `Value e
    
    method! expression dest e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      (* Context *)     
      | Pexp_tuple _
      | Pexp_construct (_, _)
      | Pexp_variant (_, _)
      | Pexp_record (_, _)
      | Pexp_array _
        ->
        begin match self#cons_context dest e with
          | `Value e ->
            fill ~loc dest e
          | `Hole e -> e
        end

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
        -> fill ~loc:e.pexp_loc dest e

      (* Type annotations *)
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
        -> fill ~loc:e.pexp_loc dest e

      (* Others/Impossible to know *) 
      | Pexp_letop _
      | Pexp_extension _
      | Pexp_unreachable
        -> fill ~loc:e.pexp_loc dest e
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
  let dest_pat, dest_expr = fresh_dest loc in
  Ast.pexp_fun ~loc Nolabel None dest_pat @@
  map_function_body (fun e -> o#expression dest_expr e) e

let entry_function_body dps_fun e =
  wrap_function_args
    (fun args body ->
       let loc = body.pexp_loc in
       [%expr 
         let d, c = Ppx_tmaa_runtime.Destiny.create () in
         Ppx_tmaa_runtime.AsyncFun.fork
           (fun () -> [%e
             Ast.pexp_apply ~loc dps_fun ((Nolabel, [%expr d])::args)
           ]);
         c
       ]
    )
    e

(* Todo: Make a real error *)
let get_vb_name vb =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name -> name
  | _ -> failwith "Forbidden binding in a let%async"

let rewrite_async_bindings
    ~(modpath : longident loc) ~dps_funs ~loc
    (r : rec_flag) (l : value_binding list) =
  let dps_funs =
    match r with
    | Nonrecursive -> dps_funs
    | Recursive -> List.fold_left (fun dps_funs vb ->
        let name = get_vb_name vb in
        DpsFuns.add name.txt dps_funs)
        dps_funs
        l        
  in
  let o = rewrite_dps modpath dps_funs in
  let f ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } as vb) =
    let dps_fun = dps_function_name @@ get_vb_name vb in
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
    Option.value ~default:(Ast_builder.Default.Located.lident ~loc "Computation")
      arg
  in
  List.concat_map
    (fun (loc, rec_flag, l) ->
       rewrite_async_bindings ~modpath ~dps_funs:DpsFuns.empty ~loc rec_flag l)
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
