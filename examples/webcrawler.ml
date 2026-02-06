open Picos

let debug = false

(** Data model and utility functions *)

module K = String

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

let () = Random.self_init ()
let pick_random_host hosts uri =
  let host = hosts.(Random.int @@ Array.length hosts) in
  Uri.with_host uri (Some host)

let process_links ~hosts ~base links =
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
  |> List.map (pick_random_host hosts)

let mk_tbl () =
  Ppx_tmaa_runtime.Memo.init ~hashed_type:(module String) ()

let%async rec fetch_page (tbl[@memo.tbl]) ~base ~cutoff ~hosts uri =
  let%memo path = Uri.path_and_query uri in
  let n = Atomic.fetch_and_add cutoff (-1) in
  if n < 0 then begin
    if debug then Format.eprintf "Cut at %a@." Uri.pp_hum uri;
    WrongPage ("cutoff")
  end else begin
    if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
    let content = fetch_uri uri in
    match content with
    | Error error -> WrongPage error
    | Ok (Html, content) ->
      let all_links = get_links content in
      let internal_links = process_links ~base ~hosts all_links in
      Page (path,
            content,
            Computation.await (
              fetch_pages tbl ~base ~cutoff ~hosts internal_links))
    | Ok (Other, content) ->
      Page (path, content, [])

  end


and fetch_pages tbl ~base ~cutoff ~hosts links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page tbl ~base ~cutoff ~hosts h) ::
       Computation.await (fetch_pages tbl ~base ~cutoff ~hosts t)
[@@warning "-32"]

(** Tree version *)

module StringTbl = Hashtbl.Make(String)

let%async rec fetch_page_tree seen ~base ~cutoff ~hosts uri =
  let path = Uri.path_and_query uri in
  if StringTbl.mem seen path then
    WrongPage ("seen")
  else
    let n = Atomic.fetch_and_add cutoff (-1) in
    if n < 0 then begin
      if debug then Format.eprintf "Cut at %a@." Uri.pp_hum uri;
      WrongPage ("cutoff")
    end else begin
      if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
      StringTbl.add seen path ();
      let content = fetch_uri uri in
      match content with
      | Error error -> WrongPage error
      | Ok (Html, content) ->
        let all_links = get_links content in
        let internal_links = process_links ~base ~hosts all_links in
        Page (path,
              content,
              Computation.await (
                fetch_pages_tree seen ~base ~cutoff ~hosts internal_links))
      | Ok (Other, content) ->
        Page (path, content, [])

  end

and fetch_pages_tree seen ~base ~cutoff ~hosts links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page_tree seen ~base ~cutoff ~hosts h) ::
       Computation.await (fetch_pages_tree seen ~base ~cutoff ~hosts t)
[@@warning "-32"]


(** Sequential versions
    Builds a table that contains the graph and serves as memoisation.
    One version in depth-first and the other breath-first
*)

type page_indir =
  | Page of string * string * Uri.t list
  | WrongPage of string
type graph = page_indir StringTbl.t

let rec fetch_page_breath (g : graph) ~base ~cutoff ~hosts (q: _ Queue.t) =
  match Queue.take_opt q with
  | None -> ()
  | Some uri when StringTbl.mem g @@ Uri.path_and_query uri ->
    fetch_page_breath g ~base ~cutoff ~hosts q
  | Some uri when !cutoff < 0 ->
    let path = Uri.path_and_query uri in
    StringTbl.add g path @@ WrongPage ("cutoff");
    fetch_page_breath g ~base ~cutoff ~hosts q
  | Some uri -> begin
      let path = Uri.path_and_query uri in
      decr cutoff;
      if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
      let content = fetch_uri uri in
      let page, follows = match content with
        | Error error -> WrongPage error, []
        | Ok (Html, content) ->
          let all_links = get_links content in
          let internal_links = process_links ~base ~hosts all_links in
          Page (path,
                content,
                internal_links),
          internal_links
        | Ok (Other, content) ->
          Page (path, content, []), []
      in
      StringTbl.add g path page;
      Queue.add_seq q @@ List.to_seq follows;
      fetch_page_breath g ~base ~cutoff ~hosts q
    end


let rec fetch_page_depth (g : graph) ~base ~cutoff ~hosts (q: _ Stack.t) =
  match Stack.pop_opt q with
  | None -> ()
  | Some uri when StringTbl.mem g @@ Uri.path_and_query uri ->
    fetch_page_depth g ~base ~cutoff ~hosts q
  | Some uri when !cutoff < 0 ->
    let path = Uri.path_and_query uri in
    StringTbl.add g path @@ WrongPage ("cutoff");
    fetch_page_depth g ~base ~cutoff ~hosts q
  | Some uri -> begin
      let path = Uri.path_and_query uri in
      decr cutoff;
      if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
      let content = fetch_uri uri in
      let page, follows = match content with
        | Error error -> WrongPage error, []
        | Ok (Html, content) ->
          let all_links = get_links content in
          let internal_links = process_links ~base ~hosts all_links in
          Page (path,
                content,
                internal_links),
          internal_links
        | Ok (Other, content) ->
          Page (path, content, []), []
      in
      StringTbl.add g path page;
      Stack.add_seq q @@ List.to_seq follows;
      fetch_page_depth g ~base ~cutoff ~hosts q
    end


(** Run everything *)

let start s = 
  Format.eprintf "Launching %s@.@?" s;
  Mtime_clock.counter ()
let stop c =
  Gc.full_major ();
  let t = Mtime_clock.count c in
  Format.printf "%a,%f,"
    Mtime.Span.pp t
    (Mtime.Span.to_float_ns t)

let mode = Sys.argv.(1) (* "seq" or a number of domains *)
let n_domains = int_of_string @@ Sys.argv.(2)
let cutoff = 10000
let uri = Uri.canonicalize @@ Uri.of_string Sys.argv.(4)
let hosts = Array.of_list @@ String.split_on_char ',' Sys.argv.(3)

let size x =
  float (Obj.reachable_words x) /. 1024. /. 1024.

let go () =
  match mode with
  | "breath" ->
    let g : graph = StringTbl.create 17 in
    let q = Queue.create () in
    Queue.push uri q;
    fetch_page_breath g ~base:uri ~cutoff:(ref cutoff) ~hosts q;
    Obj.repr g
  | "depth" ->
    let g : graph = StringTbl.create 17 in
    let q = Stack.create () in
    Stack.push uri q;
    fetch_page_depth g ~base:uri ~cutoff:(ref cutoff) ~hosts q;
    Obj.repr g
  | "tree" ->
    let seen = StringTbl.create 17 in
    let root =
      Picos_mux_multififo.run_on ~n_domains @@ fun () ->
      Computation.await @@
      fetch_page_tree seen ~base:uri ~cutoff:(Atomic.make_contended cutoff) ~hosts uri
    in
    Obj.repr root
  | "graph" ->
    let memo = mk_tbl () in
    let root =
      Picos_mux_multififo.run_on ~n_domains @@ fun () ->
      Computation.await @@
      fetch_page memo ~base:uri ~cutoff:(Atomic.make_contended cutoff) ~hosts uri
    in
    Obj.repr root
  | _ -> invalid_arg "Wrong mode"

let () =
  let title =
    Format.asprintf "Webcrawler at %a with mode=%s, node=%i, cutoff=%i, hosts=%s"
      Uri.pp_hum uri mode n_domains cutoff
      (String.concat ";" @@ Array.to_list hosts)
  in
  let c = start title in
  let o = go () in
  stop c;
  Format.printf "%.3F@.@?" (size o);
  (* Inspect.Dot.dump root_page; *)
  ()
