type socket_domain;;
type socket_type;;
type file_descr;;
type sockaddr;;
type msg_flag;;
type bytes;;
type tm;;
type shutdown_command;;
type access_permission;;
type in_channel;;
external Unix.socket : (socket_domain -> socket_type -> int -> file_descr);;
external Unix.PF_INET : socket_domain;;
external Unix.SOCK_STREAM : socket_type;;
external Unix.accept : file_descr -> file_descr * sockaddr;;
external Unix.send_substring : file_descr -> (string -> (int -> (int -> int)));;
external Unix.recv : file_descr -> (bytes -> (int -> (int -> ((msg_flag list) -> int))));;
external String.split_on_char : char -> string -> string list;;
external Bytes.create : int -> bytes;;
external Bytes.sub : bytes -> int -> int -> bytes;;
external Bytes.to_string : bytes -> string;;
external Unix.localtime : int -> tm;;
external Unix.time : unit -> int;;
external Unix.tm_min : tm -> int;;
external Unix.tm_hour : tm -> int;;
external Unix.tm_mday : tm -> int;;
external Unix.tm_mon : tm -> int;;
external Unix.tm_year : tm -> int;;
external Unix.shutdown : file_descr -> shutdown_command -> unit;;
external Unix.SHUTDOWN_ALL : shutdown_command;;
external Unix.R_OK : access_permission;;
external Unix.access : string -> (access_permission list) -> unit;;
external open_in : string -> in_channel;;
external input_line : in_channel -> string;;
external close_in : in_channel -> unit;;
external explode : string -> char list;;
external implode : char list -> string;;
external char_eq : char * char -> bool;;

let rec length l =
  match l with
  | [] -> 0
  | _::t -> 1 + (length t)
;;

let string_length s =
  length (explode s)
;;

let char_of_dig n =
  if n = 0 then '0'
  else if n = 1 then '1'
  else if n = 2 then '2'
  else if n = 3 then '3'
  else if n = 4 then '4'
  else if n = 5 then '5'
  else if n = 6 then '6'
  else if n = 7 then '7'
  else if n = 8 then '8'
  else '9'
;;

let rec reva al =
  let (a, l) = al in
  match l with
  | [] -> a
  | h::t -> reva (h::a, t)
;;

let rev l =
  reva ([], l)
;;

let rec string_of_pos_int n =
  if n < 10 then
    (char_of_dig n)::[]
  else
    let rem = n / 10 in
    let modulus = n - rem in
    (char_of_dig modulus)::(string_of_pos_int rem)
;;

let rec string_of_int n =
  implode
    (if n < 0 then
       '-'::(rev (string_of_pos_int (n * (-1))))
     else
       rev (string_of_pos_int n))
;;

let reqlog : string list ref = ref [];;
let cache : (string * string) list ref = ref [];;
let numtocache = 5;;

let sock =
  ((Unix.socket Unix.PF_INET) Unix.SOCK_STREAM) 0
;;

let accept sock =
  let (s, _) = Unix.accept sock in s
;;

let rec sendLineRec socksnlen =
  let (sock, s, n, len) = socksnlen in
  if n = len then ()
  else
    let n' = ((((Unix.send_substring sock) s) n) len) in
    sendLineRec (sock, s, n', len)
;;

let sendLine (socks : file_descr * string) =
  let (sock, s) = socks in
  let len = string_length s in
  sendLineRec (sock, s, 0, len)
;;

let recv sock =
  let n = 1024 in
  let bytes = Bytes.create n in
  let n' = (((((Unix.recv sock) bytes) 0) n) []) in
  let bytes' = ((Bytes.sub bytes) 0) n' in
  Bytes.to_string bytes'
;;

let rec split_on_char_list cl =
  let (c, l) = cl in
  match l with
  | [] -> []::[]
  | h::t ->
     let l' = split_on_char_list (c, t) in
     if char_eq (h, c) then
       []::l'
     else
       (match l' with
        | [] -> (h::[])::[]
        | hl::tl -> (h::hl)::tl)
;;

let rec map fl =
  let (f, l) = fl in
  match l with
  | [] -> []
  | h::t -> (f h)::(map (f, t))
;;

let split_on_char cs =
  let (c, s) = cs in
  let l = explode s in
  let tl = split_on_char_list (c, l) in
  map (implode, tl)
;;

let tokens s = split_on_char ('/', s);;

let rec append l1l2 =
  let (l1, l2) = l1l2 in
  match l1 with
  | [] -> l2
  | h::t -> h::(append (t, l2))
;;

let cat s1s2 =
  let (s1, s2) = s1s2 in
  let l1 = explode s1 in
  let l2 = explode s2 in
  implode (append (l1, l2))
;;

let rec cat_all l =
  match l with
  | [] -> ""
  | h::t -> cat (h, cat_all t)
;;

let cur_date_time (_: unit) =
  let tm = Unix.localtime (Unix.time ()) in
  let tm_min = Unix.tm_min tm in
  let tm_hour = Unix.tm_hour tm in
  let tm_mday = Unix.tm_mday tm in
  let tm_mon = Unix.tm_mon tm in
  let tm_year = Unix.tm_year tm in
  let pad t =
    if t < 10 then cat ("0,", (string_of_int t))
    else string_of_int t
  in
  let mon =
    if tm_mon = 0 then "Jan."
    else if tm_mon = 1 then "Feb."
    else if tm_mon = 2 then "Mar."
    else if tm_mon = 3 then "Apr."
    else if tm_mon = 4 then "May"
    else if tm_mon = 5 then "Jun."
    else if tm_mon = 6 then "Jul."
    else if tm_mon = 7 then "Aug."
    else if tm_mon = 8 then "Sep."
    else if tm_mon = 9 then "Oct."
    else if tm_mon = 10 then "Nov."
    else "Dec."
  in
  cat_all (mon::" "::(string_of_int tm_mday)::", "::(string_of_int tm_year)
           ::" "::(pad tm_hour)::":"::(pad tm_min)::[])
;;

let close sock = (Unix.shutdown sock) Unix.SHUTDOWN_ALL;;

let file_exists (s: string) =
  try
    ((Unix.access s) ((Unix.R_OK)::[]);
     true)
  with Test ->
    false
;;


let loadfile (f: string) =
  let ichan = open_in f in
  let rec load_line s =
    try
      load_line (s ^ input_line ichan)
    with End_of_file -> s
  in
  let s = load_line "" in
  close_in ichan;
  s
;;

let rec lequal l1l2 =
  let (l1, l2) = l1l2 in
  match l1 with
  | [] ->
     (match l2 with
      | [] -> true
      | a::b -> false)
  | h1::t1 ->
     (match l2 with
      | [] -> false
      | h2::t2 ->
         if char_eq (h1, h2) then
           lequal (t1, t2)
         else false)
;;

let streq ss =
  let (s1, s2) = ss in
  let sl1 = explode s1 in
  let sl2 = explode s2 in
  lequal (sl1, sl2)
;;

let rec qsort (l: (string * int) list) =
  let compare (n1n2 : (string * int) * (string * int)) =
    let (n1, n2) = n1n2 in
    let (_, n1) = n1 in
    let (_, n2) = n2 in
    n1 <= n2
  in
  let rec partition pl =
    let (p, l) = pl in
    match l with
    | [] -> ([], [])
    | h::t ->
       let (a, b) = partition (p, t) in
       if compare (h, p) then
         (h::a, b)
       else
         (a, h::b)
  in
  let rec append ls =
    let (l1, l2) = ls in
    match l1 with
    | [] -> l2
    | h::t -> (h: string * int)::(append (t, l2))
  in
    match l with
    | [] -> []
    | p::t ->
       (match t with
        | [] -> l
        | h::t2 ->
           let (le, gt) = partition (p, t) in
           let lef = future (qsort le) in
           let gtf = qsort gt in
           append (force lef, append (p::[], gtf)))
;;

let rec buildcache (cachepages : (string * string) list future * (string * int) list) =
    let (cache, pages) = cachepages in
    match pages with
    | [] -> force cache
    | p::t ->
       let (file, _) = p in
       let f = future (loadfile file) in
       let f' = future ((file, force f)::(force cache)) in
       buildcache (f', t)
;;

let redocache histogram =
  let rec updhistogram histogrampage =
    let (histogram, page) = histogrampage in
    match histogram with
    | [] -> []
    | h::t ->
       let (file, num) = h in
       if streq (file, page) then
         (file, num + 1)::t
       else
         h::(updhistogram (t, page))
  in
  let rec update_all histogramreqs =
    let (histogram, reqs) = histogramreqs in
    match reqs with
    | [] -> histogram
    | h::t ->
       update_all (updhistogram (histogram, h), t)
  in
  let rec first (ln: 'a list * int) : 'a list =
    let (l, n) = ln in
    if n <= 0 then []
    else
      match l with
      | [] -> []
      | h::t -> h::(first (t, n - 1))
  in
  let newreqs = !reqlog in
  let _ = reqlog := [] in
  let histogram' = update_all (histogram, newreqs) in
  let histogram' = qsort histogram' in
  let top = first (histogram', numtocache) in
  let newf = futref in
  let cache' = buildcache (!newf, top) in
  cache := cache';
  histogram'
;;


let rec statloop histogram : unit =
  statloop (redocache histogram)
;;

let parse_request s =
  let tokens : string list = tokens s in
  match tokens with
  | [] -> []
  | rt::r ->
     (match r with
      | [] -> []
      | url::_ -> url::[])
;;

let build_success s =
  "HTTP/1.0 200 OK"
  ::("Date: " ^ (cur_date_time ()))
  ::("Content-Type: text/html")
  ::("Content-Length: " ^ string_of_int (string_length s))
  ::""
  ::s
  ::[]
;;

let build_inv_req _ =
  "HTTP/1.0 400 Bad Request"::[]
;;

let build_404 _ =
  "HTTP/1.0 404 Not Found"::[]
;;

let rec iter (fl: (string -> unit) * string list) =
  let (f, l) = fl in
  match l with
  | [] -> ()
  | h::t -> f h; iter (f, t)
;;

let rec inploop sock =
  let req = recv sock in
  if string_length req = 0 then
    close sock
  else
    let response =
      match parse_request req with
      | [] -> build_inv_req ()
      | filename::_ ->
         if file_exists filename then
           (reqlog := filename::(!reqlog);
            build_success (loadfile filename))
         else
           build_404 ()
    in
    let sendit s = sendLine (sock, s) in
    let _ : unit = iter (sendit, response) in
    inploop sock
;;

let rec serveloop sock : unit =
  let s = accept sock in
  let _ = future (inploop s) in
  serveloop sock
;;

let main _ =
  let _ = future (serveloop sock) in
  statloop []
;;
