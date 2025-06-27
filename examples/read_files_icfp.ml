open Picos

type url = string
let check_url (_url : url) : bool Computation.t = assert false

let%async rec check_all_urls files = match files with
  | [] -> true
  | url :: rest ->
    let is_valid = Computation.await (check_url url) in
    if is_valid then
      Computation.await (check_all_urls rest)
    else false

let main () =
  let b = Computation.await @@ check_all_urls [
    "wikipedia.org";
  ] in
  assert b
  
let () = Picos_mux_fifo.run main
