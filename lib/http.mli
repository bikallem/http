(** {1 Types} *)

type request
(** [request] represents a HTTP/1.1 request *)

and method' =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]
(** [method'] represents request methods. *)

and header = string * string
(** [header] represents a HTTP header, a tuple of (name * value) *)

and response

(** {1 Request} *)

val method' : request -> method'
val target : request -> string
val http_version : request -> int * int
val headers : request -> header list
val client_addr : request -> Unix.sockaddr
val content_length : request -> int option
val body : request -> bytes

(** {1 Pretty Printers} *)

val pp_request : Format.formatter -> request -> unit
val request_to_string : request -> string

val start :
  ?domains:int -> port:int -> (Unix.file_descr * Unix.sockaddr -> unit) -> unit
(** [start ?domains ~port connection_handler] starts HTTP/1.1 server on [port].
    [connection_handler] handles client connection. *)
