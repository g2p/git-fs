open Unix
open LargeFile
open Bigarray
open Fuse

open Lwt
open Printf
open Git
open Git_types



let dir_stats = LargeFile.stat "." (* XXX *)
let file_stats = { dir_stats with
  st_nlink = 1;
  st_kind = S_REG;
  st_perm = 0o400;
  (* /proc uses zero, it works. /sys uses 4k. *)
  st_size = Int64.zero;
  }

let fname = "hello"
let name = "/" ^ fname
let contents : Fuse.buffer = Array1.of_array Bigarray.char Bigarray.c_layout
  [|'H';'e';'l';'l';'o';' ';'w';'o';'r';'l';'d';'!'|]


type hash = string

type scaff_assoc = (string * scaffolding) list
and scaff_assoc_io = unit -> scaff_assoc
and scaffolding =
  |RootScaff
  |TreesScaff
  |RefsScaff
  |FileHash of hash
  |TreeHash of hash
  |CommitHash of hash
  |OtherHash of hash (* gitlink, etc *)


let root_al = [
  "trees", TreesScaff;
  "refs", RefsScaff;
  ]

let scaffolding_child scaff child =
  match scaff with
  |RootScaff -> List.assoc child root_al
  |TreesScaff -> raise Not_found (* XXX *)
  |RefsScaff -> raise Not_found (* XXX *)
  |FileHash _ -> raise Not_found
  |TreeHash _ -> raise Not_found (* XXX *)
  |CommitHash _ -> raise Not_found (* XXX *)
  |OtherHash _ -> raise Not_found


let list_children = function
  |RootScaff -> root_al
  |TreesScaff -> [] (* XXX *)
  |RefsScaff -> [] (* XXX *)
  |FileHash _ -> raise Not_found
  |TreeHash _ -> raise Not_found (* XXX *)
  |CommitHash _ -> raise Not_found (* XXX *)
  |OtherHash _ -> raise Not_found

let is_container = function
  |RootScaff _ -> true
  |TreesScaff -> true
  |RefsScaff -> true
  |FileHash _ -> false
  |TreeHash _ -> true
  |CommitHash _ -> true
  |OtherHash _ -> false

let lookup scaff path =
  let rec lookup_r scaff = function
    |[] -> scaff
    |dir::rest ->
	lookup_r (scaffolding_child scaff dir) rest
  in match BatString.nsplit path "/" with
  |""::""::[] -> lookup_r scaff []
  |""::path_comps ->
      lookup_r scaff path_comps
  |_ -> assert false (* fuse path must start with a slash *)



let fh_data = Hashtbl.create 16
let fh_by_name = Hashtbl.create 16

let next_fh = ref 0

let lookup_and_cache path =
  try
    Hashtbl.find fh_by_name path
  with Not_found ->
    let scaff = lookup RootScaff path in
    let fh = !next_fh in
    incr next_fh;
    Hashtbl.add fh_by_name path (fh, scaff);
    Hashtbl.add fh_data fh scaff;
    (fh, scaff)

let lookup_fh fh =
  Hashtbl.find fh_data fh



let do_getattr path =
  try
    let scaff = lookup RootScaff path in
    if is_container scaff
    then dir_stats
    else file_stats
  with Not_found ->
    raise (Unix_error (ENOENT, "stat", path))

let do_opendir path flags =
  prerr_endline ("Path is: " ^ path);
  try
    let fh, scaff = lookup_and_cache path in
    Some fh
  with Not_found ->
    raise (Unix_error (ENOENT, "opendir", path))

let do_readdir path fh =
  try
    let scaff = lookup_fh fh in
    "."::".."::(List.map fst (list_children scaff))
  with Not_found ->
    assert false (* because opendir passed *)
    (*raise (Unix_error (ENOENT, "readdir", path))*)

let do_fopen path flags =
  if path = name then None
  else raise (Unix_error (ENOENT, "open", path))

let do_read path buf ofs fh =
  if path = name then
    if ofs > (Int64.of_int max_int) then 0
    else
      let ofs = Int64.to_int ofs in
      let len = min ((Array1.dim contents) - ofs) (Array1.dim buf) in
	(Array1.blit (Array1.sub contents ofs len) (Array1.sub buf 0 len);
	 len)
  else raise (Unix_error (ENOENT, "read", path))

let _ =
  main Sys.argv
    {
      default_operations with
	getattr = do_getattr;
	opendir = do_opendir;
	readdir = do_readdir;
	fopen = do_fopen;
	read = do_read;
    }
