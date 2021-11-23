let () =
  Http.start ~port:3000 (fun request ->
      let body =
        match Http.body request with
        | Some b -> Cstruct.to_string b
        | None -> ""
      in
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Http.pp_request fmt request;
      Format.fprintf fmt "\n\n%s" body;
      Http.response (Buffer.contents buf))
