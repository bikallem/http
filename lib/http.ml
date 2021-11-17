open Domainslib
module Smap = Map.Make (String)

type request = {
  method' : method';
  target : string;
  http_version : int * int;
  content_length : int option;
  headers : (string * string) list;
  client_addr : Unix.sockaddr;
  fd : Unix.file_descr;
  body : bytes;
  mutable unconsumed : Cstruct.t;
      (* unconsumed - bytes remaining after request is processed *)
}

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

and header = string * string

and response

let method' request = request.method'
let target request = request.target
let http_version request = request.http_version
let headers (request : request) = request.headers
let client_addr request = request.client_addr
let content_length request = request.content_length
let body (request : request) = request.body

(* Pretty printers *)
let rec pp_request fmt (t : request) =
  let fields =
    [
      Fmt.field "meth" (fun p -> p.method') pp_method;
      Fmt.field "target" (fun p -> p.target) Fmt.string;
      Fmt.field "http_version" (fun p -> p.http_version) pp_http_version;
      Fmt.field "headers" (fun (p : request) -> p.headers) pp_headers;
    ]
  in
  Fmt.record fields fmt t

and pp_http_version fmt t =
  let comma' fmt _ = Fmt.string fmt "," in
  Fmt.(pair ~sep:comma' int int) fmt t

and pp_headers fmt (t : header list) =
  let colon fmt _ = Fmt.string fmt ": " in
  let header_field = Fmt.(pair ~sep:colon string string) in
  Fmt.vbox Fmt.(list header_field) fmt t

and pp_method fmt t =
  (match t with
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `CONNECT -> "CONNECT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `Method s -> Format.sprintf "Method (%s)" s)
  |> Format.fprintf fmt "%s"

let request_to_string t =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  pp_request fmt t;
  Format.fprintf fmt "%!";
  Buffer.contents buf

(* let io_buffer_size = 65536 (1* UNIX_BUFFER_SIZE 4.0.0 in bytes *1) *)

let handle_client_connection (_client_sock, _client_addr) _request_handler = ()

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s

let start ?(domains = 1) ~port request_handler =
  let task_pool = Task.setup_pool ~num_additional_domains:(domains - 1) () in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let server_sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
  Unix.setsockopt server_sock Unix.SO_REUSEPORT true;
  Unix.bind server_sock listen_address;
  Unix.listen server_sock 100;
  while true do
    let client_sock, client_addr = accept_non_intr server_sock in
    ignore
    @@ Task.async task_pool (fun () ->
           handle_client_connection (client_sock, client_addr) request_handler)
  done
