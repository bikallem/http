let () =
  let port = ref 3000 in
  let domains = ref 12 in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (3000 by default)");
      ("-d", Arg.Set_int domains, " Count of domains(CPU cores)");
    ]
    ignore "An echo HTTP server";
  Http.start ~domains:!domains ~port:!port (fun _ ->
      Http.response "hello world")
