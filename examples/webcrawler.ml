open Picos

(** The actual crawler *)

type page =
  | Page of string * string * page list
  | WrongPage of string
[@@deriving eq, show { with_path = false }]

type content_type = Html | Other

(* let fetch_uri uri = *)
(*   let open Cohttp in *)
(*   let resp, body = Picos_io_cohttp.Client.get uri in *)
(*   match Response.status resp with *)
(*   | `OK -> *)
(*     let body = Cohttp.Body.to_string body in *)
(*     let content_type = *)
(*       let h = Header.get_media_type @@ Response.headers resp in *)
(*       match h with *)
(*       | Some s when String.starts_with ~prefix:"text/html" s -> Html *)
(*       | _ -> Other *)
(*     in *)
(*     Ok (content_type, body) *)
(*   | status ->  *)
(*     Error (Code.string_of_status status) *)

let fetch_uri uri =
  match Ezcurl.get ~url:(Uri.to_string uri) () with
  | Ok { code; _ } when code > 400 -> Error (Format.sprintf "code %i" code)
  | Ok { body; headers; _ } ->
    (* Format.eprintf "Headers: %a" *)
    (*   Format.(pp_print_list pp_print_string) *)
    (*   (List.map (fun (a,b) -> a ^ ": " ^ b ^ ";") headers); *)
    let content_type =
      match List.assoc_opt "Content-Type" headers with
      | Some s when String.starts_with ~prefix:"text/html" s -> Html
      | _ -> Other
    in
    Ok (content_type, body)
  | Error (_, x) -> Error x

let get_links body =
  let open Soup in
  try
    let soup = parse body in
    let links = soup $$ "a[href]" |> to_list in
    List.map (fun a -> Uri.of_string @@ R.attribute "href" a) links
  with _ -> []

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
  let n = Atomic.fetch_and_add cutoff (-1) in
  if n < 0 then begin
    (* Format.eprintf "Cut at %a@." Uri.pp_hum uri; *)
    WrongPage ("cutoff")
  end else begin
    (* Format.eprintf "Fetching %a@." Uri.pp_hum uri; *)
    let content = fetch_uri uri in
    match content with
    | Error error -> WrongPage error
    | Ok (Html, content) ->
      let all_links = get_links content in
      let internal_links = process_links ~base all_links in
      Page (Uri.path uri,
            content,
            Computation.await (
              fetch_pages tbl ~base ~cutoff internal_links))
    | Ok (Other, content) ->
      Page (Uri.path uri, content, [])

  end


and fetch_pages tbl ~base ~cutoff links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page tbl ~base ~cutoff h) ::
       Computation.await (fetch_pages tbl ~base ~cutoff t)
[@@warning "-32"]


(** Run everything *)

let start _s = 
  (* print_endline ("Launching "^s); *)
  Mtime_clock.counter ()
let stop c =
  Gc.full_major ();
  let t = Mtime_clock.count c in
  Format.printf "%a\t%f\t"
    Mtime.Span.pp t
    (Mtime.Span.to_float_ns t)

let n_domains = int_of_string Sys.argv.(1)
let cutoff = Atomic.make_contended 10000

let main () =
  let uri = Uri.canonicalize @@ Uri.of_string Sys.argv.(2) in
  let title =
    Format.asprintf "Webcrawler at %a with %i domains and cutoff=%i"
      Uri.pp_hum uri n_domains (Atomic.get cutoff)
  in
  let c = start title in
  let memo = Ppx_tmaa_runtime.Memo.init () in
  let root_page = Computation.await @@ fetch_page memo ~base:uri ~cutoff uri in
  stop c;
  Format.printf
    "%.3FMb@.@?"
    (float (Obj.reachable_words (Obj.repr root_page)) /. 1024. /. 1024.) ;
  (* Inspect.Dot.dump root_page; *)
  ()

let () =
  Picos_mux_multififo.run_on ~n_domains main
