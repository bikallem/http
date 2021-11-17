open Domainslib

let rec accept_non_intr s =
  try Unix.accept ~cloexec:true s
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s

let start ?(domains = 1) ~port connection_handler =
  let task_pool = Task.setup_pool ~num_additional_domains:(domains - 1) () in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let server_sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
  Unix.bind server_sock listen_address;
  Unix.listen server_sock 100;
  while true do
    let client_sock, client_addr = accept_non_intr server_sock in
    ignore
    @@ Task.async task_pool (fun () ->
           connection_handler (client_sock, client_addr))
  done
