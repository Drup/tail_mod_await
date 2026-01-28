open Ppxlib

module Ast = Ast_builder.Default

module DpsFuns = Map.Make(String)

let (let@) = (@@)

(** Reified module {!Ppx_tmaa_runtime.Loc}. *)
module Loc = struct
  let mk ~loc e i = 
    [%expr Ppx_tmaa_runtime.Loc.mk [%e e] [%e Ast.eint ~loc i]]

  let dummy ~loc =
    let loc = {loc with loc_ghost = true} in
    [%expr Ppx_tmaa_runtime.Loc.dummy ()]
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

module Memo = struct
  let memo ~loc ~tbl ~dest ~key ~f =
    [%expr Ppx_tmaa_runtime.Memo.memo [%e tbl] [%e dest] [%e key] [%e f]]
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

type context_info = {
  dest : expression ;
  memo_key : expression option ;
  memo_tbl : label option ;
}
let fresh_ctx loc =
  let pat, dest = fresh_dest loc in
  pat, {memo_key = None; memo_tbl = None; dest}
  
let with_new_dest ctx expr f =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let dest_pat, dest = fresh_dest loc in
  let vb = Ast.(value_binding ~loc ~pat:dest_pat ~expr) in
  Ast.(pexp_let ~loc Nonrecursive [vb] @@ f {ctx with dest})

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

(** Apply a purely syntactical wrapper over a subterm.

    Allow to easily handle constructs such as [Cons (let open X in e)].
*)
let map_subterm_if_awaits e0 f sub = match sub with
  | Expression _ | Cons _ -> Expression e0
  | Awaits mke -> Awaits (fun x -> f @@ mke x)

(** Transform the expression in tail position, given a [context_info].

    Should only be called by:
    [(new refiner modpath dps_funs)#refiner ctx e].
*)
class tail_position modpath dps_funs = object (refiner)

  val mutable bindings : value_binding list = []
  val mutable locations : expression list = []
  val mutable destinies : pattern list = []
  val mutable async_computations : expression list = []
  val mutable sync_computation : expression list = []

  (** Top level call, assemble the call to {!Destiny.refine}. *)
  method refine ctx e =
    let loc = {e.pexp_loc with loc_ghost = true} in
    let rewritten_e = refiner#rewrite_cons e in
    assert (List.length locations = List.length destinies);
    let pre_computation_bindings =
      sequential_lets (List.rev bindings)
    and post_computation_sequence =
      async_computations @ sync_computation
    in
    match rewritten_e, locations with
    | Expression e, _ ->
      pre_computation_bindings @@
      Ast.esequence ~loc (
        e ::
        post_computation_sequence)
    | Cons e, [] ->
      pre_computation_bindings @@
      Ast.esequence ~loc (
        Destiny.fill ~loc ~dest:ctx.dest e ::
        post_computation_sequence)
    | Awaits f, _ ->
      pre_computation_bindings @@
      Ast.esequence ~loc (f ctx.dest :: post_computation_sequence)
    | Cons e, _ ->
      let refine_binding = 
        let pat = Ast.plist ~loc (List.rev destinies) in
        let expr =
          pre_computation_bindings @@
          Destiny.refine ~loc ~dest:ctx.dest ~e
            ~locs:(List.rev locations)
        in
        Ast.value_binding ~loc ~pat ~expr
      in
      Ast.pexp_let ~loc Nonrecursive [refine_binding] @@
      Ast.esequence ~loc post_computation_sequence

  (** Rewrite the constructor expression [e]. *)
  method rewrite_cons e0 =
    let loc = e0.pexp_loc in
    match e0.pexp_desc with
    (* Variable *)
    | Pexp_ident _ ->
      Cons e0
    (* Constants and Unitary Constructor K or `K *)
    | Pexp_constant _
    | Pexp_construct (_, None) | Pexp_variant (_, None) ->
      Cons e0
    (* Tuple (e, ..., e) *)     
    | Pexp_tuple l ->
      let alloc l = { e0 with pexp_desc = Pexp_tuple l } in
      refiner#allocate_cons ~loc l alloc
    (* Records {e with n = e; n = e; ...} *)
    | Pexp_record (fields, with_) ->
      (* TODO Check if we can/should do something in the with_ part *)
      let names, subterms = List.split fields in
      let alloc vs =
        let fields = List.map2 (fun x y -> x,y) names vs in
        { e0 with pexp_desc = Pexp_record (fields, with_) }
      in
      refiner#allocate_cons ~loc subterms alloc
    (* Array [|e, ..., e|] *)
    | Pexp_array l ->
      let alloc l = { e0 with pexp_desc = Pexp_array l } in
      refiner#allocate_cons ~loc l alloc
    (* N-ary Constructor K (e, ... , e) *)
    | Pexp_construct
        (constructor, Some ({pexp_desc = Pexp_tuple l; _} as tup)) ->
      let alloc l =
        let tup = { tup with pexp_desc = Pexp_tuple l } in
        { e0 with pexp_desc = Pexp_construct (constructor, Some tup) }
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
    (* [await @@ e] context *)
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Lident "@@"; _} ; _},
        [Nolabel, {pexp_desc = Pexp_ident {txt = Ldot (p, "await"); _} ; _};
         Nolabel, body
        ]
      )
      ->
      if p = modpath then
        Awaits (fun dest -> refiner#rewrite_await dest body)
      else
        let s = Format.asprintf
            "This await is not in the right module.@.The module '%a' is expected but '%a' was provided.@.@.Either change namespace using the '%%YourModule.async' attribute@ or let-bind the await call before the return."
            Pprintast.longident modpath
            Pprintast.longident p
        in
        let attr = attribute_of_warning e0.pexp_loc s in
        Expression (Ast_helper.Exp.attr e0 attr)
    (* Branching control structures *)
    | Pexp_ifthenelse (b, then_, None) ->
      map_subterm_if_awaits e0
        (fun e -> Ast.pexp_ifthenelse ~loc b e None)
        (refiner#rewrite_cons then_)
    | Pexp_ifthenelse (b, then_, Some else_) ->
      begin match refiner#rewrite_cons then_, refiner#rewrite_cons else_ with
        | Awaits mkthen_, Awaits mkelse_ ->
          Awaits (fun dest ->
              Ast.pexp_ifthenelse ~loc
                b (mkthen_ dest) (Some (mkelse_ dest))
            )
        | _ -> Expression e0
      end
    (* Linear control structures. *)
    | Pexp_let (s, e1, e2) ->
      map_subterm_if_awaits e0
        (fun e -> Ast_helper.Exp.let_ ~loc s e1 e)
        (refiner#rewrite_cons e2)
    | Pexp_letmodule (s, m, e) ->
      map_subterm_if_awaits e0
        (fun e -> Ast.pexp_letmodule ~loc s m e)
        (refiner#rewrite_cons e)
    | Pexp_letexception (ec, e) ->
      map_subterm_if_awaits e0
        (fun e -> Ast.pexp_letexception ~loc ec e)
        (refiner#rewrite_cons e)
    | Pexp_open (o, e) ->
      map_subterm_if_awaits e0
        (fun e -> Ast.pexp_open ~loc o e)
        (refiner#rewrite_cons e)
    | Pexp_poly (e, ty) ->
      map_subterm_if_awaits e0
        (fun e -> Ast.pexp_poly ~loc e ty)
        (refiner#rewrite_cons e)
    | _ ->
      Expression e0

  (** [allocate_cons ~loc subterms alloc_expr] rewrite the [substerms],
      register the appropriate locations and destinies
      and build the final using [alloc_expr]. *)
  method allocate_cons ~loc ?(shift=0) subterms alloc_expr =
    let subterms = List.map refiner#rewrite_cons subterms in
    let subterms_with_tyvars =
      List.map (fun e -> e, Ast.ptyp_var ~loc:Location.none (gen_symbol ~prefix:"t_" ()))
        subterms
    in
    let alloc_pat, alloc_var = gen_var ~loc ~prefix:"v" () in
    let allocation_binding =
      let subvalues =
        List.map
          (function
            | Cons e, _ -> e
            | Expression _, ty | Awaits _, ty ->
              Ast.pexp_constraint ~loc (Loc.dummy ~loc) ty)
          subterms_with_tyvars
      in
      let expr = alloc_expr subvalues in
      Ast.value_binding ~loc ~pat:alloc_pat ~expr
    in
    bindings <- allocation_binding :: bindings;
    List.iteri (fun i e -> match e with
        | Cons _, _ ->
          ()
        | Expression computation, ty ->
          let dest_pat, dest_var = fresh_dest loc in
          let new_location =
            let ty = [%type: [%t ty] Ppx_tmaa_runtime.Loc.t] in
            Ast.pexp_constraint ~loc (Loc.mk ~loc alloc_var (shift+i)) ty
          in
          locations <- new_location :: locations;
          destinies <- dest_pat :: destinies;
          let e = Destiny.fill ~loc ~dest:dest_var computation in
          sync_computation <- e :: sync_computation;
          ()
        | Awaits f, ty ->
          let dest_pat, dest_var = fresh_dest loc in
          let new_location =
            let ty = [%type: [%t ty] Ppx_tmaa_runtime.Loc.t] in
            Ast.pexp_constraint ~loc (Loc.mk ~loc alloc_var (shift+i)) ty
          in
          locations <- new_location :: locations;
          destinies <- dest_pat :: destinies;
          let e = f dest_var in
          async_computations <- e :: async_computations;
          ()
      ) subterms_with_tyvars;
    Cons alloc_var

  (* [rewrite_await dest e] rewrite [e] when placed inside an await. *)
  method rewrite_await dest e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    (* Branching control structures *)
    | Pexp_ifthenelse (b, then_, else_) ->
      Ast.pexp_ifthenelse ~loc b
        (refiner#rewrite_await dest then_)
        (Option.map (refiner#rewrite_await dest) else_)
    | Pexp_match (matchee, cases) ->
      let cases =
        List.map (fun case ->
            { case with pc_rhs = refiner#rewrite_await dest case.pc_rhs }
          ) cases
      in
      Ast.pexp_match ~loc matchee cases
    (* Linear control structures. *)
    | Pexp_let (s, e1, e2) ->
      Ast.pexp_let ~loc s e1 (refiner#rewrite_await dest e2)
    | Pexp_letmodule (s, m, e) ->
      Ast.pexp_letmodule ~loc s m
        (refiner#rewrite_await dest e)
    | Pexp_letexception (ec, e) ->
      Ast.pexp_letexception ~loc ec
        (refiner#rewrite_await dest e)
    | Pexp_open (o, e) ->
      Ast.pexp_open ~loc o
        (refiner#rewrite_await dest e)
    | Pexp_poly (e, ty) ->
      Ast.pexp_poly ~loc (refiner#rewrite_await dest e) ty
    (* [await (f x)] context *)
    | Pexp_apply ({pexp_desc=Pexp_ident funlid; _} as funname, args)
      when DpsFuns.mem (Longident.name funlid.txt) dps_funs
      ->
      let new_fun_lid =
        Ast.Located.map_lident @@
        DpsFuns.find (Longident.name funlid.txt) dps_funs
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

(** Traversing expressions until reaching a tail position.

    Accumulates some [context_info] on the way.
*)

(* Take as argument the module path pointing to a Promise module. *)
let traverse_tail modpath dps_funs =
  object (self)
    (* Context is the destiny of the computation *)
    inherit [context_info] Ast_traverse.map_with_context as _super

    method! expression ctx e0 =
      let loc = e0.pexp_loc in
      match e0.pexp_desc with
      (* Constructor *)     
      | Pexp_tuple _
      | Pexp_construct (_, _)
      | Pexp_variant (_, _)
      | Pexp_record (_, _)
      | Pexp_array _
      (* call *)
      | Pexp_apply (_, _)
      (* Return position *)
      | Pexp_ident _
      | Pexp_constant _
      | Pexp_function _
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
        -> (new tail_position modpath dps_funs)#refine ctx e0

      (* Type annotations *)
      | Pexp_constraint (body, ty) ->
        let ty = [%type: [%t ty] Ppx_tmaa_runtime.Destiny.t] in
        let@ ctx = with_new_dest ctx
            { e0 with pexp_desc = Pexp_constraint (ctx.dest, ty) }
        in
        self#expression ctx body
      | Pexp_coerce (body, ty1, ty2) ->
        let ty1 =
          Option.map
            (fun ty1 -> [%type: [%t ty1] Ppx_tmaa_runtime.Destiny.t])
            ty1
        in
        let ty2 = [%type: [%t ty2] Ppx_tmaa_runtime.Destiny.t] in
        let@ ctx = with_new_dest ctx
          { e0 with pexp_desc = Pexp_coerce (ctx.dest, ty1, ty2) }
        in
        self#expression ctx body

      (* Memoisation *)
      | Pexp_extension ({loc; txt=("memo"|"tmaa.memo")}, payload) ->
        begin match payload, ctx.memo_tbl with
          | PStr [%str let [%p? x] = [%e? e1] in [%e? e2]], Some tbl ->
            let loc = {loc with loc_ghost = true} in
            let e2' = self#expression ctx e2 in
            Memo.memo ~tbl:(Ast.evar ~loc tbl) ~loc ~dest:ctx.dest
              ~key:e1
              ~f:(Ast.pexp_fun ~loc Nolabel None x e2')
          | _, None -> 
            Ast.pexp_extension ~loc
            @@ Location.error_extensionf ~loc
              "Memoisation is only allowed when a table is available. Please mark the parameters containing memoisation table with [@memo.tbl]"
          | _ ->
            Ast.pexp_extension ~loc
            @@ Location.error_extensionf ~loc
              "The only allowed memoisation formed is let%%memo"
        end
          
      (* Control construct, we can go down *)
      | Pexp_match (e, c) ->
        { e0 with
          pexp_desc = Pexp_match (e, self#cases ctx c)
        }
      | Pexp_ifthenelse (i, then_, else_) ->
        { e0 with
          pexp_desc =
            Pexp_ifthenelse (i, self#expression ctx then_,
                             Option.map (self#expression ctx) else_)
        }
      | Pexp_let (s, e1, e2) ->
        { e0 with
          pexp_desc = Pexp_let (s, e1, self#expression ctx e2)
        }
      | Pexp_letmodule (s, m, e) ->
        { e0 with
          pexp_desc = Pexp_letmodule (s, m, self#expression ctx e)
        }
      | Pexp_letexception (ec, e) ->
        { e0 with
          pexp_desc = Pexp_letexception (ec, self#expression ctx e)
        }
      | Pexp_open (o, e) ->
        { e0 with
          pexp_desc = Pexp_open (o, self#expression ctx e)
        }
      | Pexp_sequence (e1, e2) ->
        { e0 with
          pexp_desc = Pexp_sequence (e1, self#expression ctx e2)
        }
      | Pexp_poly (e, ty) ->
        { e0 with
          pexp_desc = Pexp_poly (self#expression ctx e, ty)
        }

      (* Wrapping construct, we give up *)
      | Pexp_try (_, _)
      | Pexp_while (_, _)
      | Pexp_for (_, _, _, _, _)
      | Pexp_assert _
        -> Destiny.fill ~loc:e0.pexp_loc ~dest:ctx.dest e0

      (* Others/Impossible to know *) 
      | Pexp_letop _
      | Pexp_extension _
      | Pexp_unreachable
        -> Destiny.fill ~loc:e0.pexp_loc ~dest:ctx.dest e0
  end

(** Traverse function arguments to modify the body.
    Leave all parameters unchanged.
*)

(* Get the memotbl in a param, if present. *)
let get_memotbl mtbl param =
  match param.pparam_desc with
  | Pparam_val (_, _, {ppat_desc = Ppat_var v; ppat_attributes; _} )
    when List.exists
        (fun attr -> attr.attr_name.txt = "memo.tbl")
        ppat_attributes ->
    begin match mtbl with
      | Some _ ->
        Location.raise_errorf ~loc:v.loc "Duplicated attribute @[memo.tbl]. Please only annotate one parameter."
      | None ->
        Some v.txt
    end
  | _ ->
    mtbl

let dps_function_body o e =
  let loc = e.pexp_loc in
  let dest_pat, ctx = fresh_ctx loc in
  let rec process_function mtbl dest_typ e = match e.pexp_desc with
    | Pexp_newtype (ty, body) ->
      { e with pexp_desc = Pexp_newtype (ty, process_function mtbl dest_typ body) }
    | Pexp_function (params, dest_typ, Pfunction_body body) ->
      let mtbl = List.fold_left get_memotbl mtbl params in
      let body = Pfunction_body (process_function mtbl dest_typ body) in
      { e with pexp_desc = Pexp_function (params, None, body) }
    | Pexp_function (params, typ, Pfunction_cases (cases, loc, attrs)) ->
      let mtbl = List.fold_left get_memotbl mtbl params in
      let cases =
        List.map
          (fun case ->
             {case with pc_rhs = process_function mtbl dest_typ case.pc_rhs})
          cases
      in
      let body = Pfunction_cases (cases, loc, attrs) in
      { e with pexp_desc = Pexp_function (params, typ, body) }
    | _ ->
      (* Found the body *)
      let loc = e.pexp_loc in
      let ctx = {ctx with memo_tbl = mtbl} in
      let typ_cstr = match dest_typ with
        | None -> ctx.dest
        | Some Pconstraint ty ->
          let ty = [%type: [%t ty] Ppx_tmaa_runtime.Destiny.t] in
          Ast.pexp_constraint ~loc ctx.dest ty
        | Some Pcoerce (ty1, ty2) ->
          let ty1 =
            Option.map
              (fun ty1 -> [%type: [%t ty1] Ppx_tmaa_runtime.Destiny.t])
              ty1
          in
          let ty2 = [%type: [%t ty2] Ppx_tmaa_runtime.Destiny.t] in
          Ast.pexp_coerce ~loc ctx.dest ty1 ty2
      in
      Ast.pexp_let ~loc Nonrecursive
        [Ast.value_binding ~loc ~pat:dest_pat ~expr:typ_cstr] @@
      o#expression ctx e
  in 
  Ast.pexp_fun ~loc Nolabel None dest_pat @@ process_function None None e

(** Wrap a function definition to insert `async` in the body. *)
let rec wrap_function arg_list mtbl f e = match e.pexp_desc with
  | Pexp_newtype (_ty, body) ->
    wrap_function arg_list mtbl f body
  | Pexp_function (params, typ, Pfunction_body body) ->
    let params, args =
      List.split @@
      List.filter_map (function
          | { pparam_desc = Pparam_newtype _; _ } -> None
          | { pparam_loc = loc ; pparam_desc = Pparam_val (lbl, _, p) } ->
            let name = gen_symbol ~prefix:"arg$" () in
            let attrs = p.ppat_attributes in
            let pat =
              Ast_helper.Pat.var ~attrs ~loc @@ Ast.Located.mk ~loc name
            in
            let arg =
              Ast_helper.Exp.ident ~attrs ~loc @@
              Ast.Located.lident ~loc name in
            let param =
              { pparam_loc = loc ; pparam_desc = Pparam_val (lbl, None, pat) }
            in
            Some (param, (lbl, arg))
        ) params
    in
    let mtbl = List.fold_left get_memotbl mtbl params in
    let body = wrap_function (arg_list@args) mtbl f body in
    { e with pexp_desc = Pexp_function (params, typ, Pfunction_body body) }
  | _ ->
    (* Found the body *)
    f arg_list mtbl e
let wrap_function_args = wrap_function []

let entry_function_body dps_fun e =
  wrap_function_args None
    (fun args mtbl body ->
       let loc = body.pexp_loc in
       let e_mtbl = match mtbl with
         | None -> [%expr Ppx_tmaa_runtime.Memo.init ()]
         | Some v -> Ast.evar ~loc v
       in
       [%expr 
         let d, c = Ppx_tmaa_runtime.Destiny.mk [%e e_mtbl] in
         Ppx_tmaa_runtime.AsyncFun.fork
           (fun () -> [%e
             Ast.pexp_apply ~loc dps_fun ((Nolabel, [%expr d])::args)
           ]);
         c
       ]
    )
    e

let get_vb_name vb =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name -> name
  | _ ->
    Location.raise_errorf ~loc:vb.pvb_loc "Forbidden binding in a let%%async. All bindings should be functions declarations."

let dps_function_name =
  Ast.Located.map (fun orig -> orig ^ "$dps")

let rewrite_async_bindings
    ~(modpath : longident) ~dps_funs ~loc
    (r : rec_flag) (l : value_binding list) =
  let dps_funs =
    match r with
    | Nonrecursive -> dps_funs
    | Recursive -> List.fold_left (fun dps_funs vb ->
        let name = get_vb_name vb in
        let dps_name = dps_function_name name in
        DpsFuns.add name.txt dps_name dps_funs)
        dps_funs
        l        
  in
  let o = traverse_tail modpath dps_funs in
  let f ({ pvb_pat; pvb_expr; _ } as vb) =
    let dps_fun = DpsFuns.find (get_vb_name vb).txt dps_funs in
    let vb =
      let loc = vb.pvb_loc in
      let dps_fun_expr =
        Ast.pexp_ident ~loc @@ Ast.Located.map_lident dps_fun
      in
      let pvb_expr = entry_function_body dps_fun_expr pvb_expr in
      { vb with pvb_expr }
    and vb_dps =
      let loc = pvb_pat.ppat_loc in
      let pvb_pat = Ast.ppat_var ~loc dps_fun in
      let pvb_expr = dps_function_body o pvb_expr in
      { vb with pvb_pat ; pvb_attributes = []; pvb_expr }
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

let expander ~loc:_ ~path:_ ~arg bindings =
  let modpath =
    arg
    |> Option.map (fun {txt;_} -> txt)
    |> Option.value ~default:(lident "Computation")
  in
  List.concat_map
    (fun (loc, rec_flag, l) ->
       rewrite_async_bindings ~modpath ~dps_funs:DpsFuns.empty ~loc rec_flag l)
    bindings

let ext = Extension.declare_inline_with_path_arg
    "async"
    context
    extracter
    expander

let () =
  Driver.register_transformation
    ~extensions:[ext]
    "tail_mod_await"
