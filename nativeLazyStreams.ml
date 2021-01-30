(*
                             CS51 Lab 15
                         Native Lazy Streams

   A native implementation of lazy streams with some useful functions.
 *)

type 'a stream_internal = Cons of 'a * 'a stream
 and 'a stream = 'a stream_internal Lazy.t ;;

let head (s : 'a stream) : 'a =
  let Cons (h, _) = Lazy.force s in h ;;
  
let tail (s : 'a stream) : 'a stream =
  let Cons (_, t) = Lazy.force s in t ;;
  
let rec first (n : int) (s : 'a stream) : 'a list =
  if n = 0 then []
  else head s :: first (n - 1) (tail s) ;;

let rec smap (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  failwith "smap native not implemented" ;;

let rec smap2 (f : 'a -> 'b -> 'c)
              (s1 : 'a stream)
              (s2 : 'b stream)
            : 'c stream = 
  failwith "smap2 native not implemented" ;;

let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
  failwith "sfilter native not implemented" ;;
