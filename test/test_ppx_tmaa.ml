type 'a tree = N of 'a * 'a tree * 'a tree | L of int

let%async rec map f t : 'a tree = match t with
  | L n -> L n
  | N (v , tl , tr ) ->
    let tr = map f tr in
    N (f v, Eio.Promise.await (map f tl), Eio.Promise.await tr)
