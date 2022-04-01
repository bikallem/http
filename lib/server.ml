open Domainslib

let rec handle_request reader writer fd request_handler =
  Printf.printf "\nhandle_request%!";
  match Request.parse reader with
  | request ->
      Printf.printf "\nrequest%!";
      let response = request_handler request in
      Response.write response writer;
      Printf.printf "\nwrote response%!";
      if Request.is_keep_alive request then (
        (if not request.read_complete then
         match Http.Header.get_transfer_encoding (Request.headers request) with
         | Http.Transfer.Fixed _ -> ignore @@ Request.read_fixed request
         | Http.Transfer.Chunked -> ignore @@ Request.read_chunk request ignore
         | _ -> ());
        handle_request reader writer fd request_handler)
      else Unix.close fd
  | exception End_of_file -> Unix.close fd
  | exception Parser.Parse_failure _e ->
      Response.(write bad_request writer);
      Unix.close fd
  | exception _ ->
      Response.(write internal_server_error writer);
      Unix.close fd

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s

let domain_count =
  match Sys.getenv_opt "HTTPD_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

let start ?(domains = domain_count) ~port request_handler =
  let task_pool = Task.setup_pool ~num_additional_domains:(domains - 1) () in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let server_sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
  Unix.setsockopt server_sock Unix.SO_REUSEPORT true;
  Unix.bind server_sock listen_address;
  Unix.listen server_sock 10_000;
  while true do
    let fd, _client_addr = accept_non_intr server_sock in
    let reader = Reader.create 0x1000 fd in
    let writer = Writer.create fd in
    let write_task () = Writer.run writer in
    let request_task () = handle_request reader writer fd request_handler in
    Printf.printf "\nRequest_task%!";
    ignore @@ Task.async task_pool request_task;
    Printf.printf "\nWrite_task%!";
    ignore @@ Task.async task_pool write_task; 
  done
