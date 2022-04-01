open Dhttp

let () =
  Server.start ~port:8080 (fun req ->
      match Request.resource req with
      | "/" ->
          let body =
            match Request.read_fixed req with Ok s -> s | Error _ -> ""
          in
          let buf = Buffer.create 0 in
          let fmt = Format.formatter_of_buffer buf in
          Request.pp fmt req;
          Format.fprintf fmt "\n\n%s" body;
          Response.text (Buffer.contents buf)
      | _ -> Response.not_found)
