open Ppxlib

let dps_transformation _ l =
  l (* TODO *)


let context = Extension.Context.structure_item
let extracter =
  Ast_pattern.(pstr @@ many @@ map2 ~f:(fun x y -> x,y) @@ pstr_value __ __)

let expander ~ctxt ~arg bindings =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let promise_mod_path =
    Option.value ~default:(Ast_builder.Default.Located.lident ~loc "Promise")
      arg
  in
  List.map
    (fun l ->
       let rec_flag, l = dps_transformation promise_mod_path l in
       Ast_builder.Default.(pstr_value ~loc rec_flag l))
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
