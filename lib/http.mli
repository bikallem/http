val start :
  ?domains:int -> port:int -> (Unix.file_descr * Unix.sockaddr -> unit) -> unit
(** [start ?domains ~port connection_handler] starts HTTP/1.1 server on [port].
    [connection_handler] handles client connection. *)
