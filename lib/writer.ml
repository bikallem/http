open Domainslib

type t = { fd : Unix.file_descr; buf : string Chan.t }

let create fd =
  let buf = Chan.make_unbounded () in
  { fd; buf }

let write_string t s = Chan.send t.buf s

let rec run t =
  match Chan.recv t.buf with
  | s ->
      let b = Bytes.of_string s in
      ignore @@ Unix.write t.fd b 0 (Bytes.length b);
      run t
  | exception _ -> ()
