type 'a t = Reader.t -> 'a

exception Parse_failure of string

let return v _ = v
let fail err _ = Stdlib.raise_notrace (Parse_failure err)
let commit rdr = Reader.commit rdr
let ( <?> ) p err rdr = try p rdr with Parse_failure _e -> fail err rdr

let ( >>= ) p f rdr =
  let a = p rdr in
  f a rdr

let ( let* ) = ( >>= )

let ( >>| ) p f rdr =
  let v = p rdr in
  f v

let ( let+ ) = ( >>| )

let ( <* ) p q rdr =
  let a = p rdr in
  let _ = q rdr in
  a

let ( *> ) p q rdr =
  let _ = p rdr in
  q rdr

let ( <|> ) p q rdr =
  let old_pos = Reader.pos rdr in
  let old_committed = Reader.committed_bytes rdr in
  try p rdr
  with Parse_failure _ as ex ->
    if old_committed < Reader.committed_bytes rdr then raise_notrace ex
    else (
      rdr.pos <- old_pos;
      q rdr)

let lift f p = p >>| f

let lift2 f p q rdr =
  let a = p rdr in
  let b = q rdr in
  f a b

let rec ensure rdr len =
  if Reader.(length rdr < pos rdr + len) then (
    ignore (Reader.fill rdr len);
    ensure rdr len)

let pos rdr = Reader.pos rdr

let end_of_input rdr =
  try
    ensure rdr 1;
    false
  with End_of_file -> true

let option : 'a -> 'a t -> 'a t = fun x p -> p <|> return x

let peek_char rdr =
  let open Reader in
  if pos rdr < length rdr then unsafe_get rdr (pos rdr)
  else (
    ensure rdr 1;
    unsafe_get rdr rdr.pos)

let peek_string n rdr =
  try
    ensure rdr n;
    Reader.substring rdr ~off:rdr.pos ~len:n
  with End_of_file -> fail "[peek_string] not enough input" rdr

let sprintf = Printf.sprintf

let char c rdr =
  let c' = peek_char rdr in
  if c = c' then Reader.incr_pos rdr
  else fail (sprintf "[char] expected %C, got %C" c c') rdr

let any_char rdr =
  ensure rdr 1;
  let c = Reader.unsafe_get rdr rdr.pos in
  Reader.incr_pos rdr;
  c

let satisfy f rdr =
  let c = peek_char rdr in
  if f c then (
    Reader.incr_pos rdr;
    c)
  else fail "[satisfy]" rdr

let string s rdr =
  let len = String.length s in
  ensure rdr len;
  let pos = pos rdr in
  let i = ref 0 in
  while
    !i < len
    && Char.equal (Reader.unsafe_get rdr (pos + !i)) (String.unsafe_get s !i)
  do
    incr i
  done;
  if len = !i then Reader.incr_pos ~n:len rdr else fail "[string]" rdr

let fix f =
  let rec p = lazy (f r) and r inp = (Lazy.force p) inp in
  r

let count_while rdr f =
  let i = ref 0 in
  let continue = ref true in
  while !continue do
    try
      ensure rdr (!i + 1);
      let c = Reader.(unsafe_get rdr (pos rdr + !i)) in
      if f c then incr i else continue := false
    with End_of_file -> continue := false
  done;
  !i

let take_while1 f rdr =
  let count = count_while rdr f in
  if count < 1 then fail "[take_while1] count is less than 1" rdr
  else
    let s = Reader.(substring rdr ~off:(pos rdr) ~len:count) in
    Reader.incr_pos ~n:count rdr;
    s

let take_while f rdr =
  let count = count_while rdr f in
  if count > 0 then (
    let s = Reader.(substring rdr ~off:(pos rdr) ~len:count) in
    Reader.incr_pos ~n:count rdr;
    s)
  else ""

let take_bigstring : int -> Bigstringaf.t t =
 fun n rdr ->
  try
    ensure rdr n;
    let s = Reader.(copy rdr ~off:(pos rdr) ~len:n) in
    Reader.incr_pos ~n rdr;
    s
  with End_of_file -> fail "[take_bigstring] not enough input" rdr

let take : int -> string t =
 fun n rdr ->
  try
    ensure rdr n;
    let s = Reader.(substring rdr ~off:(pos rdr) ~len:n) in
    Reader.incr_pos ~n rdr;
    s
  with End_of_file -> fail "[take] not enough input" rdr

let take_till f = take_while (fun c -> not (f c))

let rec many : 'a t -> 'a list t =
 fun p rdr ->
  try
    let a = p rdr in
    a :: many p rdr
  with Parse_failure _ | End_of_file -> []

let rec many_till : 'a t -> _ t -> 'a list t =
 fun p t rdr ->
  try
    let _ = t rdr in
    let a = p rdr in
    a :: many_till p t rdr
  with Parse_failure _ -> []

let skip f rdr =
  ensure rdr 1;
  let c = Reader.(unsafe_get rdr (pos rdr)) in
  if f c then Reader.incr_pos rdr else fail "[skip]" rdr

let skip_while f rdr =
  let count = count_while rdr f in
  Reader.incr_pos ~n:count rdr

let rec skip_many p rdr =
  match p rdr with _ -> skip_many p rdr | exception Parse_failure _ -> ()
