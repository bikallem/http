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

and response_code
(** See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
    details on http response codes *)

and handler = request -> response

and middleware = handler -> handler

(** {1 Request} *)

val method' : request -> method'
val target : request -> string
val http_version : request -> int * int
val headers : request -> header list
val client_addr : request -> Unix.sockaddr
val content_length : request -> int
val body : request -> Cstruct.t

(** {1 Pretty Printers} *)

val pp_request : Format.formatter -> request -> unit
val request_to_string : request -> string

(** {1 Response} *)

val ok : response_code
(** HTTP 200 response code *)

val internal_server_error : response_code
(** HTTP 500 (Internal Server Error) response code *)

val bad_request : response_code
(** HTTP 400 Bad Request response code *)

val response_code : ?reason_phrase:string -> int -> response_code
(** [response code] returns {!type:response_code} represented by [code]. It
    raises exception if [code] is not a valid HTTP code.

    [reason_pharse] default value is "unknown". This is the reason phrase used
    if [code] is not a standard http response code.

    See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for valid
    response codes. *)

val code_int : response_code -> int
(** [response_code_int response_code] returns an integer representation of
    [response_code]. *)

val code_reason_phrase : response_code -> string
(** [response_code_reason_phrase response_code] returns reason phrase for
    [response_code]. *)

val response :
  ?response_code:response_code -> ?headers:header list -> string -> response
(** [response ~response_code ~headers body] returns a {!type:response}. The
    default value for [response_code] is [200]. *)

val response_bigstring :
  ?response_code:response_code ->
  ?headers:header list ->
  Cstruct.buffer ->
  response
(** [response_bigstring] similar to {!type:response} except body is a bigstring. *)

(* val text : string -> response Lwt.t *)
(** [text body] creates a response with HTTP status 200 and content-type of
    ["text/plain; charset=utf-8"]. *)

(* val html : string -> response Lwt.t *)
(** [text body] creates a response with HTTP status 200 and content-type of
    ["text/html; charset=UTF-8"]. *)

(* val tyxml : Tyxml.Html.doc -> response Lwt.t *)

val start : ?domains:int -> port:int -> handler -> unit
(** [start ?domains ~port connection_handler] starts HTTP/1.1 server on [port].
    [connection_handler] handles client connection. *)
