open Picos

let debug = false

(** Data model and utility functions *)

module K = struct
  type t = Uri.t
  let equal x y = String.equal (Uri.path x) (Uri.path y)
  let hash x = String.hash (Uri.path x)
end

type page =
  | Page of Uri.t * string * page list
  | WrongPage of Uri.t * string
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

let get_internal_links base content =
  process_links ~base @@ get_links content

let mk_tbl () =
  Ppx_tmaa_runtime.Memo.init ~hashed_type:(module K) ()

let%async rec fetch_page (tbl[@memo.tbl]) ~base ~cutoff uri =
  let%memo _path = uri in
  let n = Atomic.fetch_and_add cutoff (-1) in
  if n < 0 then begin
    if debug then Format.eprintf "Cut at %a@." Uri.pp_hum uri;
    WrongPage (uri, "cutoff")
  end else begin
    if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
    match fetch_uri uri with
    | Error error -> WrongPage (uri, error)
    | Ok (content_type, content) ->
      let links = match content_type with
        | Html -> get_internal_links base content
        | Other -> []
      in
      Page (uri, content,
            Computation.await (fetch_pages tbl ~base ~cutoff links))
  end


and fetch_pages tbl ~base ~cutoff links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page tbl ~base ~cutoff h) ::
       Computation.await (fetch_pages tbl ~base ~cutoff t)
[@@warning "-32"]

(** Tree version *)

module H = Saturn.Htbl

let%async rec fetch_page_tree seen ~base ~cutoff uri =
  let path = uri in
  if not (H.try_add seen path ()) then
    WrongPage (uri, "seen")
  else
    let n = Atomic.fetch_and_add cutoff (-1) in
    if n < 0 then begin
      if debug then Format.eprintf "Cut at %a@." Uri.pp_hum uri;
      WrongPage (uri, "cutoff")
    end else begin
      if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
      match fetch_uri uri with
      | Error error -> WrongPage (uri, error)
      | Ok (Html, content) ->
        let uris = get_internal_links base content in
        Page (uri,
              content,
              Computation.await (
                fetch_pages_tree seen ~base ~cutoff uris))
      | Ok (Other, content) ->
        Page (uri, content, [])
    end

and fetch_pages_tree seen ~base ~cutoff links = match links with
  | [] -> []
  | h :: t ->
     Computation.await (fetch_page_tree seen ~base ~cutoff h) ::
       Computation.await (fetch_pages_tree seen ~base ~cutoff t)
[@@warning "-32"]

(** Manual versions
    Builds a table that contains the graph and serves as memoisation.
*)
type page_indir =
  | Page of string * Uri.t list
  | WrongPage of string
  | Placeholder
type graph = (string, page_indir) H.t

let rec fetch_page_async_manual (g : graph) ~base ~cutoff uri =
  let path = Uri.path uri in
  if H.try_add g path Placeholder then
    let n = Atomic.fetch_and_add cutoff (-1) in
    let v, next = if n < 0 then begin
        if debug then Format.eprintf "Cut at %a@." Uri.pp_hum uri;
        WrongPage "cutoff", []
      end else begin
        if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
        match fetch_uri uri with
        | Error error ->
          WrongPage error, []
        | Ok (Html, content) ->
          let uris = get_internal_links base content in
          Page (content, uris), uris
        | Ok (Other, content) ->
          Page (content, []), []
      end
    in
    let _ = H.set_exn g path v in
    List.iter (fun uri ->
        Ppx_tmaa_runtime.AsyncFun.fork @@ fun () ->
        fetch_page_async_manual g ~base ~cutoff uri)
      next

(** Sequential versions
    Builds a table that contains the graph and serves as memoisation.
    One version in depth-first and the other breath-first
*)

module Tbl = Hashtbl.Make(K)

let rec fetch_page_breath g ~base ~cutoff (q: _ Queue.t) =
  match Queue.take_opt q with
  | None -> ()
  | Some uri when Tbl.mem g uri ->
    fetch_page_breath g ~base ~cutoff q
  | Some uri when !cutoff < 0 ->
    Tbl.add g uri @@ WrongPage "cutoff";
    fetch_page_breath g ~base ~cutoff q
  | Some uri -> begin
      decr cutoff;
      if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
      let page, follows = match fetch_uri uri with
        | Error error -> WrongPage error, []
        | Ok (Html, content) ->
          let uris = get_internal_links base content in
          Page (content, uris), uris
        | Ok (Other, content) ->
          Page (content, []), []
      in
      Tbl.add g uri page;
      Queue.add_seq q @@ List.to_seq follows;
      fetch_page_breath g ~base ~cutoff q
    end


let rec fetch_page_depth g ~base ~cutoff (q: _ Stack.t) =
  match Stack.pop_opt q with
  | None -> ()
  | Some uri when Tbl.mem g uri ->
    fetch_page_depth g ~base ~cutoff q
  | Some uri when !cutoff < 0 ->
    Tbl.add g uri @@ WrongPage "cutoff";
    fetch_page_depth g ~base ~cutoff q
  | Some uri -> begin
      decr cutoff;
      if debug then Format.eprintf "Fetching %a@." Uri.pp_hum uri;
      let page, follows = match fetch_uri uri with
        | Error error -> WrongPage error, []
        | Ok (Html, content) ->
          let uris = get_internal_links base content in
          Page (content, uris), uris
        | Ok (Other, content) ->
          Page (content, []), []
      in
      Tbl.add g uri page;
      Stack.add_seq q @@ List.to_seq follows;
      fetch_page_depth g ~base ~cutoff q
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

let mode = Sys.argv.(1)
let n_domains = int_of_string @@ Sys.argv.(2)
let cutoff = 5000
let uri = Uri.canonicalize @@ Uri.of_string Sys.argv.(3)

let size x =
  float (Obj.reachable_words x) /. 1024. /. 1024.

(* let run f = *)
(*   let pool = Moonpool.Ws_pool.create ~num_threads:n_domains () in *)
(*   let v = Moonpool.Runner.run_wait_block pool f in *)
(*   Moonpool.Runner.shutdown pool; *)
(*   v *)

let run f =
  Picos_mux_multififo.run_on ~n_domains f

let go () =
  match mode with
  | "breath" ->
    let g = Tbl.create 17 in
    let q = Queue.create () in
    Queue.push uri q;
    fetch_page_breath g ~base:uri ~cutoff:(ref cutoff) q;
    Obj.repr g
  | "depth" ->
    let g = Tbl.create 17 in
    let q = Stack.create () in
    Stack.push uri q;
    fetch_page_depth g ~base:uri ~cutoff:(ref cutoff) q;
    Obj.repr g
  | "manual" ->
    let g = H.create ~hashed_type:(module String) () in
    let () =
      run @@ fun () ->
      fetch_page_async_manual g ~base:uri ~cutoff:(Atomic.make_contended cutoff) uri
    in
    Obj.repr g
  | "tree" ->
    let seen = H.create ~hashed_type:(module K) () in
    let root =
      run @@ fun () ->
      Computation.await @@
      fetch_page_tree seen ~base:uri ~cutoff:(Atomic.make_contended cutoff) uri
    in
    Obj.repr root
  | "graph" ->
    let memo = mk_tbl () in
    let root =
      run @@ fun () ->
      Computation.await @@
      fetch_page memo ~base:uri ~cutoff:(Atomic.make_contended cutoff) uri
    in
    Obj.repr root
  | _ -> invalid_arg "Wrong mode"

let () =
  let title =
    Format.asprintf "Webcrawler at %a with mode=%s, node=%i, cutoff=%i"
      Uri.pp_hum uri mode n_domains cutoff
  in
  let c = start title in
  let o = go () in
  stop c;
  Format.printf "%.3F@.@?" (size o);
  (* Inspect.Dot.dump root_page; *)
  ()
