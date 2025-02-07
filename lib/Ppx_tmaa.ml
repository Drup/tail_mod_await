open Ppxlib

module Ast = Ast_builder.Default

module DpsFuns = Set.Make(String)

(** Reified module {!Ppx_tmaa_runtime.Loc}. *)
module Loc = struct
  let mk ~loc e i = 
    [%expr Ppx_tmaa_runtime.Loc.mk [%e e] [%e Ast.eint ~loc i]]

  let dummy ~loc =
    let loc = {loc with loc_ghost = true} in
    [%expr Ppx_tmaa_runtime.Loc.dummy]
end

(** Reified module {!Ppx_tmaa_runtime.Destiny}. *)
module Destiny = struct
  let refine ~loc ~dest ~e ~locs =
    [%expr Ppx_tmaa_runtime.Destiny.refine
        [%e dest] [%e e] [%e Ast.elist ~loc locs]]
  
  let fill ~loc ~dest e =
    [%expr Ppx_tmaa_runtime.Destiny.fill [%e dest] [%e e]]

  let chain ~loc ~dest e =
    [%expr Ppx_tmaa_runtime.Destiny.chain [%e e] [%e dest]]
end

let rec sequential_lets vbs body = match vbs with
  | [] -> body
  | vb :: t ->
    Ast.pexp_let ~loc:vb.pvb_loc Nonrecursive [vb] @@
    sequential_lets t body

let symbol_id = ref 0
let gen_symbol =
  fun ?(prefix = "_x") () ->
    symbol_id := !symbol_id + 1;
    Printf.sprintf "%s__%03i_" prefix !symbol_id
let reset_ids () = symbol_id := 0

let gen_var ~loc ~prefix () =
  let symb = gen_symbol ~prefix () in
  let e = Ast.evar ~loc symb in
  let pat = Ast.pvar ~loc symb in
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

(** A subterm of a constructor. *)
type subterm =
  (** The substerm only has awaits in terminal positions.
      Carry the function which, given the variable holding a destiny,
      builds the target code.
  *)
  | Awaits of (expression -> expression)
  (** The subterm is an immediate value (potentially with holes). *)
  | Cons of expression
  (** An arbitrary subterm. *)
  | Expression of expression

(** A [refiner] turns a constructor in terminal position into a call to refine.

    Should only be called by:
    [(new refiner modpath dps_funs)#refiner dest e].
*)
class refiner modpath dps_funs = object (refiner)

  val mutable bindings : value_binding list = []
  val mutable locations : expression list = []
  val mutable destinies : pattern list = []
  val mutable async_computations : expression list = []
  val mutable sync_computation : expression list = []

  (** Top level call, assemble the call to {!Destiny.refine}. *)
  method refine dest e =
    let loc = {e.pexp_loc with loc_ghost = true} in
    let value = match refiner#rewrite_cons e with
      | Expression _ -> assert false
      | Awaits f -> f dest
      | Cons e -> e
    in
    assert (List.length locations = List.length destinies);
    if List.is_empty locations then
      sequential_lets (List.rev bindings) @@
      Ast.esequence ~loc (
        Destiny.fill ~loc ~dest value ::
        async_computations @
        sync_computation)
    else
      let refine_binding =
        let pat = Ast.plist ~loc (List.rev destinies) in
        let expr =
          sequential_lets (List.rev bindings) @@
          Destiny.refine ~loc ~dest ~e:value ~locs:(List.rev locations)
        in
        Ast.value_binding ~loc ~pat ~expr
      in
      Ast.pexp_let ~loc Nonrecursive [refine_binding] @@
      Ast.esequence ~loc (async_computations @ sync_computation)

  (** Rewrite the constructor expression [e]. *)
  method rewrite_cons e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    (* Constants and Unitary Constructor K or `K *)
    | Pexp_constant _
    | Pexp_construct (_, None) | Pexp_variant (_, None) ->
      Cons e
    (* Tuple (e, ..., e) *)     
    | Pexp_tuple l ->
      let alloc l = { e with pexp_desc = Pexp_tuple l } in
      refiner#allocate_cons ~loc l alloc
    (* Records {e with n = e; n = e; ...} *)
    | Pexp_record (fields, with_) ->
      (* TODO Check if we can/should do something in the with_ part *)
      let names, subterms = List.split fields in
      let alloc vs =
        let fields = List.map2 (fun x y -> x,y) names vs in
        { e with pexp_desc = Pexp_record (fields, with_) }
      in
      refiner#allocate_cons ~loc subterms alloc
    (* Array [|e, ..., e|] *)
    | Pexp_array l ->
      let alloc l = { e with pexp_desc = Pexp_array l } in
      refiner#allocate_cons ~loc l alloc
    (* N-ary Constructor K (e, ... , e) *)
    | Pexp_construct
        (constructor, Some ({pexp_desc = Pexp_tuple l; _} as tup)) ->
      let alloc l =
        let tup = { tup with pexp_desc = Pexp_tuple l } in
        { e with pexp_desc = Pexp_construct (constructor, Some tup) }
      in
      refiner#allocate_cons ~loc l alloc
    (* Constructor K e -- not a tuple *)
    | Pexp_construct (constructor, Some e) ->
      let alloc l = match l with
        | [e] -> { e with pexp_desc = Pexp_construct (constructor, Some e) }
        | _ -> assert false
      in
      refiner#allocate_cons ~loc [e] alloc
    | Pexp_variant (constructor, Some e) -> 
      let alloc l = match l with
        | [e] -> { e with pexp_desc = Pexp_variant (constructor, Some e) }
        | _ -> assert false
      in
      refiner#allocate_cons ~loc ~shift:1 [e] alloc
    (* [await e] context *)
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Ldot (p, "await"); _} ; _},
        [Nolabel, body]
      )
      ->
      if p = modpath then
        Awaits (refiner#rewrite_await body)
      else
        let s = Format.asprintf
            "This await is not in the right module.@.The module '%a' is expected but '%a' was provided.@.@.Either change namespace using the '%%YourModule.async' attribute@ or let-bind the await call before the return."
            Pprintast.longident modpath
            Pprintast.longident p
        in
        let attr = attribute_of_warning e.pexp_loc s in
        Expression (Ast_helper.Exp.attr e attr)
    | _ ->
      Expression e

  (** [allocate_cons ~loc subterms alloc_expr] rewrite the [substerms],
      register the appropriate locations and destinies
      and build the final using [alloc_expr]. *)
  method allocate_cons ~loc ?(shift=0) subterms alloc_expr =
    let subterms = List.map refiner#rewrite_cons subterms in
    let alloc_pat, alloc_var = gen_var ~loc ~prefix:"v" () in
    let allocation_binding =
      let subvalues =
        List.map
          (function Cons e -> e | Expression _ | Awaits _ -> Loc.dummy ~loc)
          subterms
      in
      let expr = alloc_expr subvalues in
      Ast.value_binding ~loc ~pat:alloc_pat ~expr
    in
    bindings <- allocation_binding :: bindings;
    List.iteri (fun i e -> match e with
        | Cons _ ->
          ()
        | Expression computation ->
          let dest_pat, dest_var = fresh_dest loc in
          locations <- Loc.mk ~loc alloc_var (shift+i) :: locations;
          destinies <- dest_pat :: destinies;
          let e = Destiny.fill ~loc ~dest:dest_var computation in
          sync_computation <- e :: sync_computation;
          ()
        | Awaits f ->
          let dest_pat, dest_var = fresh_dest loc in
          locations <- Loc.mk ~loc alloc_var (shift+i) :: locations;
          destinies <- dest_pat :: destinies;
          let e = f dest_var in
          async_computations <- e :: async_computations;
          ()
      ) subterms;
    Cons alloc_var
  
  (* [rewrite_await dest e] rewrite [e] when placed inside an await. *)
  method rewrite_await e dest =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    (* [await (f x)] context *)
    | Pexp_apply ({pexp_desc=Pexp_ident funlid; _} as funname, args)
      when DpsFuns.mem (Longident.name funlid.txt) dps_funs
      ->
      let new_fun_lid =
        Ast.Located.map_lident @@
        dps_function_name @@ Ast.Located.map Longident.name funlid
      in
      let dps_application = 
        {e with
         pexp_desc =
           Pexp_apply ({funname with pexp_desc=Pexp_ident new_fun_lid},
                       (Nolabel,dest)::args)}
      in
      [%expr Ppx_tmaa_runtime.AsyncFun.fork (fun () -> [%e dps_application])]
    (* [await e] context *)
    | _ ->
      Destiny.chain ~loc e ~dest
end

(** Transformation to destiny passing style.

    Take as argument the module path pointing to a Promise module. *)
let rewrite_dps modpath dps_funs =
  object (self)
    (* Context is the destiny of the computation *)
    inherit [_] Ast_traverse.map_with_context as _super

    method! expression dest e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      (* Constructor *)     
      | Pexp_tuple _
      | Pexp_construct (_, _)
      | Pexp_variant (_, _)
      | Pexp_record (_, _)
      | Pexp_array _
        -> (new refiner modpath dps_funs)#refine dest e
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
        -> Destiny.fill ~loc:e.pexp_loc ~dest e

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
        -> Destiny.fill ~loc:e.pexp_loc ~dest e

      (* Others/Impossible to know *) 
      | Pexp_letop _
      | Pexp_extension _
      | Pexp_unreachable
        -> Destiny.fill ~loc:e.pexp_loc ~dest e
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
         let d, c = Ppx_tmaa_runtime.Destiny.mk () in
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
    ~(modpath : longident) ~dps_funs ~loc
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

let expander ~ctxt:_ ~arg bindings =
  let modpath =
    arg
    |> Option.map (fun {txt;_} -> txt)
    |> Option.value ~default:(lident "Computation")
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
