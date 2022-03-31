type t =
  | GET
  | POST
  | HEAD
  | DELETE
  | PATCH
  | PUT
  | OPTIONS
  | TRACE
  | CONNECT
  | Other of string

let to_string : t -> string = function
  | GET -> "GET"
  | POST -> "POST"
  | HEAD -> "HEAD"
  | DELETE -> "DELETE"
  | PATCH -> "PATCH"
  | PUT -> "PUT"
  | OPTIONS -> "OPTIONS"
  | TRACE -> "TRACE"
  | CONNECT -> "CONNECT"
  | Other s -> s

let of_string : string -> t = function
  | "GET" -> GET
  | "POST" -> POST
  | "HEAD" -> HEAD
  | "DELETE" -> DELETE
  | "PATCH" -> PATCH
  | "PUT" -> PUT
  | "OPTIONS" -> OPTIONS
  | "TRACE" -> TRACE
  | "CONNECT" -> CONNECT
  | s -> Other s

let compare (a : t) (b : t) = Stdlib.compare a b
