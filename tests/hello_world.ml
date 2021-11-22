let () =
  Http.start ~port:3000 (fun request ->
      let body = Http.body request |> Option.get in
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Http.pp_request fmt request;
      Format.fprintf fmt "\n\n%s" (Cstruct.to_string body);
      Http.response (Buffer.contents buf))
