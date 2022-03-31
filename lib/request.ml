type t = {
  reader : Reader.t;
  headers : Header.t;
  mutable version : Version.t;
  mutable meth : Method.t;
  mutable resource : string;
  mutable read_complete : bool;
}

let reader t = t.reader
let headers t = t.headers
let meth t = t.meth
let resource t = t.resource
let version t = t.version

let is_keep_alive t =
  match Header.find_opt t.headers "connection" with
  | Some "close" -> false
  | Some "keep-alive" -> true
  | _ -> Version.(compare t.version HTTP_1_1) >= 0

let create ?(initial_header_len = 15) reader =
  {
    version = Version.HTTP_1_1;
    headers = Header.create initial_header_len;
    meth = Method.Other "";
    reader;
    resource = "";
    read_complete = false;
  }

let clear t = Header.clear t.headers

module P = Parser

let token =
  P.take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let ows = P.skip_while (function ' ' | '\t' -> true | _ -> false)
let crlf = P.string "\r\n"
let is_cr = function '\r' -> true | _ -> false
let space = P.char '\x20'
let p_meth = P.(token <* space >>| Method.of_string)
let p_resource = P.(take_while1 (fun c -> c != ' ') <* space)

let p_version =
  P.(
    string "HTTP/1." *> any_char <* crlf >>= function
    | '1' -> return Version.HTTP_1_1
    | '0' -> return Version.HTTP_1_0
    | v -> fail (Format.sprintf "Invalid HTTP version: %C" v))

let p_header =
  P.(
    lift2
      (fun key value -> (key, value))
      (token <* char ':' <* ows)
      (take_till is_cr <* crlf))

let rec p_headers : Header.t -> unit P.t =
 fun hdrs inp ->
  p_header inp |> Header.add_header hdrs;
  match P.peek_char inp with '\r' -> crlf inp | _ -> p_headers hdrs inp

let parse_into (t : t) =
  match P.end_of_input t.reader with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      t.meth <- p_meth t.reader;
      t.resource <- p_resource t.reader;
      t.version <- p_version t.reader;
      p_headers t.headers t.reader;
      P.commit t.reader

let read_fixed t =
  if t.read_complete then Error "End of file"
  else
    match Header.find_opt t.headers "content-length" with
    | Some v -> (
        try
          let content_length = int_of_string v in
          let content = Parser.take content_length t.reader in
          Ok content
        with e -> Error (Printexc.to_string e))
    | None -> Error "Request is not a fixed content body"
