let () =
  let port = ref 3000 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number (3000 by default)") ]
    ignore "A hello-world HTTP server";
  Http.start ~port:!port (fun _ -> Http.response "hello world")
