open Eio

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let%async rec map f = function
  | L -> L
  | N (v , tl , tr ) ->
    let tl = map f tl and tr = map f tr in
    N (f v, Promise.await tl, Promise.await tr)
