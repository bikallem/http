type t = HTTP_1_1 | HTTP_1_0

let to_string = function HTTP_1_0 -> "HTTP/1.0" | HTTP_1_1 -> "HTTP/1.1"

let of_string = function
  | "HTTP/1.0" -> HTTP_1_0
  | "HTTP/1.1" -> HTTP_1_1
  | v -> failwith ("HTTP version not supported: " ^ v)

let compare (a : t) (b : t) = Stdlib.compare a b
