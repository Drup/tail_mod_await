type 'a tree = N of 'a * 'a tree * 'a tree | L of int

let%async rec map f t : 'a tree = match t with
  | L n -> L n
  | N (v , tl , tr ) ->
    let tl = map f tl and tr = map f tr in
    N (f v, Eio.Promise.await tl, Eio.Promise.await tr)
