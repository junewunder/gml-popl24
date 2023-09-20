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
external Unix.shutdown : file_descr -> shutdown_command -> unit;;
external Unix.SHUTDOWN_ALL : shutdown_command;;
external Unix.R_OK : access_permission;;
external Unix.access : string -> (access_permission list) -> unit;;
external string_of_int : int -> string;;
external open_in : string -> in_channel;;
external input_line : in_channel -> string;;
external close_in : in_channel -> unit;;
external split_on_char : char * string -> string list;;
external streq : string * string -> bool;;
external string_length : string -> int;;
external cat : string * string -> string;;
external sendLine : file_descr * string -> unit;;
external recv : file_descr -> string;;
external cur_date_time : unit -> string;;

let reqlog : string list ref = ref [];;
let cache : (string * string) list ref = ref [];;
let numtocache = 5;;

let sock =
  ((Unix.socket Unix.PF_INET) Unix.SOCK_STREAM) 0
;;

let accept sock =
  let (s, _) = Unix.accept sock in s
;;

let tokens s = split_on_char ('/', s);;

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
  let cache' = buildcache (future [], top) in
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
