open Picos

(** The actual crawler *)

type page =
  | Page of string * string * page list
  | WrongPage of string
[@@deriving eq, show { with_path = false }]

(* let fetch_uri uri = *)
(*   let resp, body = Picos_io_cohttp.Client.get uri in *)
(*   if Cohttp.Response.status resp != `OK then failwith "Not OK"; *)
(*   Cohttp.Body.to_string body *)

let fetch_uri uri =
  match Ezcurl.get ~url:(Uri.to_string uri) () with
  | Ok { code; _ } when code > 400 -> Error (Format.sprintf "code %i" code)
  | Ok { body; _ } -> Ok body
  | Error (_, x) -> Error x

let is_page uri =
  Uri.path uri = ""
  || String.ends_with ~suffix:".html" @@ Uri.path uri
  || String.ends_with ~suffix:".htm" @@ Uri.path uri
  || String.ends_with ~suffix:"/" @@ Uri.path uri
  

let get_links body =
  let open Soup in
  try
    let soup = parse body in
    let links = soup $$ "a[href]" |> to_list in
    List.map (fun a -> Uri.of_string @@ R.attribute "href" a) links
  with Soup.Parse_error _ -> []

let process_links ~base links =
  List.filter_map (fun link ->
      let link =
        Uri.resolve "http" base link
        |> Uri.with_uri ~query:None ~fragment:None
      in
      let base_host = Uri.host base in
      if Uri.host link = base_host then
        Some link
      else
        None
    ) links
  |> List.sort_uniq Uri.compare

let%async rec fetch_page (tbl[@memo.tbl]) ~base ~cutoff uri =
  let%memo _uris = Uri.to_string uri in
  if cutoff <= 0 then begin
    Format.eprintf "Cut at %a@." Uri.pp_hum uri;
    WrongPage ("cutoff")
  end else begin
    Format.eprintf "Fetching %a@." Uri.pp_hum uri;
    let content = fetch_uri uri in
    match content with
    | Error error -> WrongPage error
    | Ok content ->
      if is_page uri then
        let all_links = get_links content in
        let internal_links = process_links ~base all_links in
        Page (Uri.path uri,
              content,
              Computation.await (
                fetch_pages tbl ~base ~cutoff:(cutoff-1) internal_links))
      else
        Page (Uri.path uri, content, [])
        
  end


and fetch_pages tbl ~base ~cutoff links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page tbl ~base ~cutoff h) ::
       Computation.await (fetch_pages tbl ~base ~cutoff t)
[@@warning "-32"]

let main () =
  let cutoff = 5 in
  let uri = Uri.canonicalize @@ Uri.of_string (Sys.argv.(1)) in
  let memo = Ppx_tmaa_runtime.Memo.init () in
  let root_page = Computation.await @@ fetch_page memo ~base:uri ~cutoff uri in
  Format.eprintf
    "Total size : %.3FMb@."
    (float (Obj.reachable_words (Obj.repr root_page)) /. 1024. /. 1024.)
  ;
  let context =
    object
      inherit Inspect.Dot.context () as _c
      (* method! should_follow_edge ~src:_ ~field:_ ~dst = *)
      (*   if Obj.tag dst = Obj.string_tag then false else true *)
      (* method! should_inline obj = *)
      (*   if Obj.tag obj = Obj.string_tag then true else c#should_inline obj *)
    end
  in
  Inspect.Dot.dump ~context root_page

let () =
  Picos_mux_multififo.run_on ~n_domains:4 main
