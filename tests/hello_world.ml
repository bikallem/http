open Dhttp

let () = Server.start ~port:3000 (fun _request -> Response.text "hello world")
