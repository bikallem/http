open Domainslib

type request = {
  method' : method';
  target : string;
  http_version : int * int;
  content_length : int;
  headers : (string * string) list;
  client_addr : Unix.sockaddr;
  fd : Unix.file_descr;
  mutable body : Cstruct.t;
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

exception Request_error of string

(* Request *)

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

(* parsing HTTP request *)
(*-- https://datatracker.ietf.org/doc/html/rfc7230#appendix-B --*)

open Angstrom

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let space = char '\x20'
let htab = char '\t'
let ows = skip_many (space <|> htab)
let optional x = option None (x >>| Option.some)
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let crlf = string_ci "\r\n" <?> "[crlf]"

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let header_fields =
  let header_field =
    let* header_name = token <* char ':' <* ows >>| String.lowercase_ascii in
    let+ header_value =
      let field_content =
        let c2 =
          optional
            (let+ c1 = skip_many1 (space <|> htab) *> vchar in
             Format.sprintf " %c" c1)
          >>| function
          | Some s -> s
          | None -> ""
        in
        lift2 (fun c1 c2 -> Format.sprintf "%c%s" c1 c2) vchar c2
      in
      many field_content >>| String.concat "" <* crlf <* commit
    in
    (header_name, header_value)
  in
  many header_field

let method_of_string meth =
  match String.uppercase_ascii meth with
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "DELETE" -> `DELETE
  | "CONNECT" -> `CONNECT
  | "OPTIONS" -> `OPTIONS
  | "TRACE" -> `TRACE
  | _ -> `Method meth

(*-- request-line = method SP request-target SP HTTP-version CRLF -- *)
let request_line =
  let* meth = token >>| method_of_string <* space in
  let* request_target = take_while1 (fun c -> c != ' ') <* space in
  let digit = satisfy (function '0' .. '9' -> true | _ -> false) in
  let* http_version =
    let* major = string "HTTP/" *> digit <* char '.' in
    let* minor = digit <* crlf in
    if Char.equal major '1' && Char.equal minor '1' then return (1, 1)
    else fail (Format.sprintf "Invalid HTTP version: (%c,%c)" major minor)
  in
  commit *> return (meth, request_target, http_version)

let io_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 in bytes *)

let request_error fmt =
  Format.ksprintf (fun err -> raise (Request_error err)) fmt

let read_body request =
  let content_length = content_length request in
  let unconsumed_length = Cstruct.length request.unconsumed in
  if content_length = 0 then Cstruct.empty
  else if content_length = unconsumed_length then request.unconsumed
  else if content_length < unconsumed_length then (
    let sz = unconsumed_length - content_length in
    let buf = Cstruct.(sub request.unconsumed 0 content_length) in
    let unconsumed = Cstruct.sub request.unconsumed content_length sz in
    request.unconsumed <- unconsumed;
    buf)
  else
    let sz = content_length - unconsumed_length in
    let buf = Cstruct.create sz in
    let sz' = ExtUnix.All.BA.read request.fd buf.buffer in
    let buf = if sz' <> sz then Cstruct.sub buf 0 sz' else buf in
    if unconsumed_length > 0 then Cstruct.append request.unconsumed buf else buf

let request fd unconsumed client_addr =
  let request_or_eof =
    let request' =
      (let* method', target, http_version = request_line in
       let+ headers = header_fields in
       let content_length =
         match List.assoc_opt "content-length" headers with
         | Some len -> (
             try int_of_string len
             with _ -> request_error "Invalid content-length value: %s" len)
         | None -> request_error "content-length header not found"
       in
       let request =
         {
           method';
           target;
           http_version;
           content_length;
           headers;
           client_addr;
           fd;
           body = Cstruct.empty;
           unconsumed;
         }
       in
       `Request request)
      <* crlf
    in
    let eof = end_of_input >>| fun () -> `Connection_closed in
    request' <|> eof
  in
  let rec parse_request = function
    | Buffered.Partial k ->
        let unconsumed_length = Cstruct.length unconsumed in
        let len = io_buffer_size - unconsumed_length in
        let buf = Cstruct.create len in
        let len' = ExtUnix.All.BA.read fd buf.buffer in
        if len' = 0 then parse_request (k `Eof)
        else
          let buf = if len' != len then Cstruct.sub buf 0 len' else buf in
          let bigstring =
            (if unconsumed_length > 0 then Cstruct.(append unconsumed buf)
            else buf)
            |> Cstruct.to_bigarray
          in
          parse_request (k (`Bigstring bigstring))
    | Buffered.Done ({ off; len; buf }, x) -> (
        let unconsumed =
          if len > 0 then Cstruct.of_bigarray ~off ~len buf else Cstruct.empty
        in
        match x with
        | `Request (req : request) ->
            req.body <- read_body req;
            req.unconsumed <- unconsumed;
            `Request req
        | x -> x)
    | Buffered.Fail (_, marks, err) ->
        `Error (String.concat " > " marks ^ ": " ^ err)
  in
  parse_request (Buffered.parse request_or_eof)

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
