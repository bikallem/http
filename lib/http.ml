open Domainslib

let _debug_on =
  ref
    (match String.trim @@ Sys.getenv "HTTP_DBG" with
    | "" -> false
    | _ ->
        Printexc.record_backtrace true;
        true
    | exception _ -> false)

(* Are we in a test environment. Need this so that our tests don't fail un-necessarily due to
   datetime value. *)
let _test_on =
  ref
    (match String.trim (Sys.getenv "HTTP_TEST") with
    | "" -> false
    | _ -> true
    | exception _ -> false)

let debug k =
  if !_debug_on then
    k (fun fmt ->
        Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") stdout fmt)

type request = {
  method' : method';
  target : string;
  http_version : int * int;
  content_length : int;
  headers : (string * string) list;
  client_addr : Unix.sockaddr;
  client_fd : Unix.file_descr;
  cookies : Http_cookie.t list Lazy.t;
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

module Smap = Map.Make (String)

type response = {
  response_code : response_code;
  headers : header list;
  cookies : Http_cookie.t Smap.t;
  body : bytes;
}

and response_code = int * string
(* code, reason phrase *)

and handler = request -> response
and middleware = handler -> handler

exception Request_error of string

(* Request *)

let method' request = request.method'
let target request = request.target
let http_version request = request.http_version
let headers (request : request) = request.headers
let client_addr request = request.client_addr
let content_length request = request.content_length
let body (request : request) = request.body

let cookies (request : request) =
  Lazy.force request.cookies
  |> List.map (fun cookie -> (Http_cookie.name cookie, cookie))

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

(* Response *)
let response_code ?(reason_phrase = "unknown") = function
  (* Informational *)
  | 100 -> (100, "Continue")
  | 101 -> (101, "Switching Protocols")
  (* Successful *)
  | 200 -> (200, "OK")
  | 201 -> (201, "Created")
  | 202 -> (202, "Accepted")
  | 203 -> (203, "Non-Authoritative Information")
  | 204 -> (204, "No Content")
  | 205 -> (205, "Reset Content")
  | 206 -> (206, "Partial Content")
  (* Redirection *)
  | 300 -> (300, "Multiple Choices")
  | 301 -> (301, "Moved Permanently")
  | 302 -> (302, "Found")
  | 303 -> (303, "See Other")
  | 304 -> (304, "Not Modified")
  | 305 -> (305, "Use Proxy")
  | 306 -> (306, "Temporary Redirect")
  (* Client error *)
  | 400 -> (400, "Bad Request")
  | 401 -> (401, "Unauthorized")
  | 402 -> (402, "Payment Required")
  | 403 -> (403, "Forbidden")
  | 404 -> (404, "Not Found")
  | 405 -> (405, "Method Not Allowed")
  | 406 -> (406, "Not Acceptable")
  | 407 -> (407, "Proxy Authentication Required")
  | 408 -> (408, "Request Timeout")
  | 409 -> (409, "Conflict")
  | 410 -> (410, "Gone")
  | 411 -> (411, "Length Required")
  | 412 -> (412, "Precondition Failed")
  | 413 -> (413, "Payload Too Large")
  | 414 -> (414, "URI Too Long")
  | 415 -> (415, "Unsupported Media Type")
  | 416 -> (416, "Range Not Satisfiable")
  | 417 -> (417, "Expectation Failed")
  | 418 -> (418, "I'm a teapot") (* RFC 2342 *)
  | 420 -> (420, "Enhance Your Calm")
  | 426 -> (426, "Upgrade Required")
  (* Server error *)
  | 500 -> (500, "Internal Server Error")
  | 501 -> (501, "Not Implemented")
  | 502 -> (502, "Bad Gateway")
  | 503 -> (503, "Service Unavailable")
  | 504 -> (504, "Gateway Timeout")
  | 505 -> (505, "HTTP Version Not Supported")
  | c ->
      if c < 0 then failwith (Printf.sprintf "code: %d is negative" c)
      else if c < 100 || c > 999 then
        failwith (Printf.sprintf "code: %d is not a three-digit number" c)
      else (c, reason_phrase)

let code_int : response_code -> int = fun (code, _) -> code
let code_reason_phrase (_, phrase) = phrase
let ok = response_code 200
let internal_server_error = response_code 500
let bad_request = response_code 400

let response ?(response_code = ok) ?(headers = []) body =
  {
    response_code;
    headers =
      List.map
        (fun (name, value) -> (String.lowercase_ascii name, value))
        headers;
    cookies = Smap.empty;
    body = Bytes.unsafe_of_string body;
  }

let text body =
  response ~response_code:ok
    ~headers:[ ("content-type", "text/plain; charset=UTF-8") ]
    body

let html body =
  response ~response_code:ok
    ~headers:[ ("content-type", "text/html; charset=UTF-8") ]
    body

(* Request *)

let request (client_addr, client_fd) unconsumed =
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
      let sz' = ExtUnix.All.BA.read request.client_fd buf.buffer in
      let buf = if sz' <> sz then Cstruct.sub buf 0 sz' else buf in
      if unconsumed_length > 0 then Cstruct.append request.unconsumed buf
      else buf
  in
  let request_or_eof =
    let request' =
      (let* method', target, http_version = request_line in
       let+ headers = header_fields in
       let content_length =
         match List.assoc_opt "content-length" headers with
         | Some len -> (
             try int_of_string len
             with _ -> request_error "Invalid content-length value: %s" len)
         | None -> 0
       in
       let request =
         {
           method';
           target;
           http_version;
           content_length;
           headers;
           client_addr;
           client_fd;
           cookies =
             lazy
               (match List.assoc_opt "cookie" headers with
               | Some v -> (
                   match Http_cookie.of_cookie v with
                   | Ok cookies -> cookies
                   | Error e -> request_error "%s" e)
               | None -> []);
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
        let len' = ExtUnix.All.BA.single_read client_fd buf.buffer in
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

(** [to_rfc1123 t] converts [t] to a string in a format as defined by RFC 1123. *)
let datetime_to_string (tm : Unix.tm) =
  let weekday =
    match tm.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | 7 -> "Sun"
    | _ -> assert false
  in
  let month =
    match tm.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> assert false
  in
  Format.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday tm.tm_mday month
    (1900 + tm.tm_year) tm.tm_hour tm.tm_min tm.tm_sec

let epoch_time =
  Unix.
    {
      tm_sec = 0;
      tm_min = 0;
      tm_hour = 0;
      tm_mday = 0;
      tm_mon = 0;
      tm_year = 0;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }

(* TODO replace this with eio vector version when available. *)
let write_response fd { response_code; headers; body; cookies } =
  let buf = Buffer.create io_buffer_size in

  (* Write response status line. *)
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" (code_int response_code)
      (code_reason_phrase response_code)
  in
  Buffer.add_string buf status_line;

  (* Add set-cookie headers if we have cookies. *)
  let headers =
    if Smap.cardinal cookies > 0 then
      let headers =
        Smap.fold
          (fun _cookie_name cookie headers ->
            ("set-cookie", Http_cookie.to_set_cookie cookie) :: headers)
          cookies headers
      in
      (* Update cache-control header so that we don't cache cookies. *)
      let no_cache = {|no-cache="Set-Cookie"|} in
      let cache_control_hdr = "cache-control" in
      match List.assoc_opt cache_control_hdr headers with
      | Some hdr_value ->
          let headers = List.remove_assoc cache_control_hdr headers in
          (cache_control_hdr, Format.sprintf "%s, %s" hdr_value no_cache)
          :: headers
      | None -> (cache_control_hdr, no_cache) :: headers
    else headers
  in

  let body_len = Bytes.length body in

  (* Add content-length headers if it doesn't exist. *)
  let headers =
    if List.exists (fun (hdr, _) -> hdr = "content-length") headers then headers
    else ("content-length", string_of_int body_len) :: headers
  in

  (* Add Date header. *)
  let headers =
    if List.exists (fun (hdr, _) -> hdr = "date") headers then headers
    else 
      let datetime = if !_test_on then epoch_time else Unix.(time () |> gmtime) in 
      ("date", datetime_to_string datetime):: headers
  in

  (* Write response headers. *)
  List.iter
    (fun (name, v) ->
      let hdr = Format.sprintf "%s: %s\r\n" name v in
      Buffer.add_string buf hdr)
    headers;

  (* Write response body. *)
  Buffer.add_string buf "\r\n";

  if body_len > 0 then Buffer.add_bytes buf body;

  Cstruct.of_string (Buffer.contents buf)
  |> Cstruct.to_bigarray
  |> ExtUnix.All.BA.single_write fd
  |> ignore

let handle_client_connection (client_addr, client_fd) request_handler =
  let handle_request (req : request) =
    match request_handler req with
    | response ->
        write_response client_fd response;
        `Next_request
    | exception exn ->
        (match exn with
        | Request_error error ->
            debug (fun k -> k "Request error: %s" error);
            write_response client_fd (response ~response_code:bad_request "")
        | exn ->
            debug (fun k -> k "Exception: %s" (Printexc.to_string exn));
            write_response client_fd
              (response ~response_code:internal_server_error ""));
        `Close_connection
  in
  let rec loop_requests unconsumed =
    match request (client_addr, client_fd) unconsumed with
    | `Request request -> (
        debug (fun k -> k "%s\n%!" (request_to_string request));
        match handle_request request with
        | `Close_connection -> Unix.close client_fd
        | `Next_request -> (loop_requests [@tailcall]) request.unconsumed)
    | `Connection_closed -> Unix.close client_fd
    | `Error e ->
        debug (fun k ->
            k "Error while parsing request: %s\nClosing connection" e);
        write_response client_fd (response ~response_code:bad_request "");
        Unix.close client_fd
  in
  loop_requests Cstruct.empty

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s

let cpu_count () =
  try
    match Sys.os_type with
    | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
    | _ -> (
        let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
        let close () = ignore (Unix.close_process_in i) in
        try
          let in_channel = Scanf.Scanning.from_channel i in
          Scanf.bscanf in_channel "%d" (fun n ->
              close ();
              n)
        with e ->
          close ();
          raise e)
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _ | End_of_file
  | Unix.Unix_error (_, _, _)
  ->
    1

let start ?(domains = cpu_count ()) ~port request_handler =
  (* TODO num_additional_domains needs to be greater than 1 it seems. *)
  let task_pool = Task.setup_pool ~num_additional_domains:(domains - 1) () in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let server_sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
  Unix.setsockopt server_sock Unix.SO_REUSEPORT true;
  Unix.bind server_sock listen_address;
  Unix.listen server_sock 10_000;
  while true do
    let fd, client_addr = accept_non_intr server_sock in
    (fun () -> handle_client_connection (client_addr, fd) request_handler)
    |> Task.async task_pool
    |> ignore
  done
