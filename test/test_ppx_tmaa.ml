open Eio

type 'a tree = N of 'a * 'a tree * 'a tree | L

let%async rec map f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let tl = map f tl and tr = map f tr in
    N (f v, Promise.await tl, Promise.await tr)
