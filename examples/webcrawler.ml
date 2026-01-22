open Picos

(** The actual crawler *)

type page =
  | Page of string * page list
  | Document of string
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
  let soup = parse body in
  let links = soup $$ "a[href]" |> to_list in
  List.map (fun a -> Uri.of_string @@ R.attribute "href" a) links

let process_links ~base links =
  List.filter_map (fun link ->
      let link =
        Uri.resolve "http" base link
        |> Uri.with_uri ~query:None ~fragment:None
      in
      let base_host = Uri.host base and base_path = Uri.path base in
      if Uri.host link = base_host &&
           String.sub (Uri.path link) 0 (String.length base_path) = base_path
      then
        Some link
      else
        None
    ) links

let%async rec fetch_page ~base ~cutoff uri =
  let uri = Uri.canonicalize uri in
  Fiber.sleep ~seconds:0.1;
  let%memo _uris = Uri.to_string uri in
  if not @@ is_page uri then begin
    Format.eprintf "Found %a@." Uri.pp_hum uri;
    Document (Uri.path uri)
  end else if cutoff <= 0 then begin
    Format.eprintf "Cut at %a@." Uri.pp_hum uri;
    WrongPage ("cutoff")
  end else begin
    Format.eprintf "Fetching %a@." Uri.pp_hum uri;
    let content = fetch_uri uri in
    match content with
    | Error error -> WrongPage error
    | Ok content -> 
      let all_links = get_links content in
      let internal_links = process_links ~base all_links in
      Page (Uri.path uri,
            Computation.await (
              fetch_pages ~base ~cutoff:(cutoff-1) internal_links))
  end

and fetch_pages ~base ~cutoff links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page ~base ~cutoff h) ::
       Computation.await (fetch_pages ~base ~cutoff t)
[@@warning "-32"]

let main () =
  let cutoff = 3 in
  let uri = Uri.of_string (Sys.argv.(1)) in
  let root_page = Computation.await @@ fetch_page ~base:uri ~cutoff uri in
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
  (* ignore root_page *)

let () =
  Picos_mux_multififo.run_on ~n_domains:4 main
