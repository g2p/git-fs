(* vim: set tw=0 sw=2 ts=2 et : *)

open Unix
open LargeFile
open Bigarray
open Fuse


(* lwt would sprinkle this module with monads everywhere, nip it in the bud. *)
let wait_on_monad monad =
  (* let waiter, wakener = Lwt.wait () in *)
  (* XXX Lwt_main.run not thread-safe. Or just not reentrant, which is OK. *)
  Lwt_main.run monad

(* Going through the shell is evil/ugly,
 * but I didn't find a non system()-based api *)
(* Unlike a shell backtick, doesn't remove trailing newlines *)
let backtick shell_cmd =
  prerr_endline (Printf.sprintf "Command “%S”" shell_cmd); flush_all ();
  let out_pipe = BatUnix.open_process_in shell_cmd in
  BatIO.read_all out_pipe

let trim_endline str =
  (* XXX not what the spec says, this trims both ends *)
  BatString.trim str

let git_wt = "/home/g2p/var/co/git/ocaml-git" (* XXX *)
let git_dir = git_wt ^ "/.git"

let repo = wait_on_monad (Git.Repo.repo git_wt)

let backtick_git cmd =
  let cmd_str = String.concat " " ("git"::"--git-dir"::git_dir::cmd) in
  backtick cmd_str

let dir_stats = LargeFile.stat "." (* XXX *)
let file_stats = { dir_stats with
  st_nlink = 1;
  st_kind = S_REG;
  st_perm = 0o400;
  (* /proc uses zero, it works. /sys uses 4k. *)
  st_size = Int64.zero;
  }
let exe_stats = { file_stats with
  st_perm = 0o500;
  }
let symlink_stats = { file_stats with
  st_kind = S_LNK;
  }

let contents : Fuse.buffer = Array1.of_array Bigarray.char Bigarray.c_layout
  [|'H';'e';'l';'l';'o';' ';'w';'o';'r';'l';'d';'!'|]


type hash = string

type scaffolding =
  |RootScaff
  |TreesScaff
  |RefsScaff
  |CommitsScaff
  |RefScaff of string
  |Symlink of string
  |PlainBlob of hash
  |ExeBlob of hash
  |TreeHash of hash
  |CommitHash of hash
  |CommitMsg of hash
  |CommitParents of hash
  |OtherHash of hash (* gitlink, etc *)


(* association list for the fs root *)
let root_al = [
  "trees", TreesScaff;
  "refs", RefsScaff;
  "commits", CommitsScaff;
  ]

let slash_free s =
  (*prerr_endline (Printf.sprintf "Encoding “%S”" s);*)
  (* urlencode as soon as I find a module that implements it *)
  (* Str.global_replace (Str.regexp_string "/") "-" s *)
  BatBase64.str_encode s

let reslash s =
  let r = BatBase64.str_decode s in
  (*prerr_endline (Printf.sprintf "Decoding into “%S”" r);*)
  r

let commit_of_ref ref =
  (*
  let opts = `BoolOpt ("hash", true) :: `BoolOpt ("verify", true) :: `Bare ref
  :: [] in
  let stdout s = s in
  wait_on_monad (repo#git#exec ~stdout "show-ref" opts);
  *)
  trim_endline (backtick_git [ "show-ref"; "--hash"; "--verify"; "--"; ref ])

let tree_of_commit_with_prefix hash prefix =
  (* prefix should be empty or a relative path with no initial slash
   * and no . or .. *)
  trim_endline (backtick_git [ "rev-parse"; "--revs-only"; "--no-flags";
  "--verify"; "--quiet"; hash ^ "^{tree}" ^ ":" ^ prefix ])

let tree_of_commit hash =
  tree_of_commit_with_prefix hash ""

let rec parents_depth depth =
  if depth = 0 then ""
  else "../" ^ parents_depth (depth - 1)

let commit_symlink_of_ref ref depth =
  let to_root = parents_depth depth in
  Symlink (to_root ^ "commits/" ^ (commit_of_ref ref))

let tree_symlink_of_commit hash depth =
  let to_root = parents_depth depth in
  Symlink (to_root ^ "trees/" ^ (tree_of_commit hash))

let fh_data = Hashtbl.create 16
let fh_by_name = Hashtbl.create 16

let next_fh = ref 0

let prime_cache path scaff =
  try
    Hashtbl.find fh_by_name path
  with Not_found ->
    let fh = !next_fh in
    incr next_fh;
    Hashtbl.add fh_by_name path (fh, scaff);
    Hashtbl.add fh_data fh scaff;
    (fh, scaff)

let tree_children_uncached hash =
  let lines = backtick_git [ "ls-tree"; "-z"; "--"; hash; ] in
  let rgx = Str.regexp "\\(100644 blob\\|100755 blob\\|040000 tree\\) \\([0-9a-f]+\\)\t\\([^\000]+\\)\000" in
  let rec parse lines offset =
    if String.length lines = offset then []
    else if not (Str.string_match rgx lines offset)
    then failwith (
      Printf.sprintf "Ill-formatted ls-tree lines: %S" (
        BatString.slice ~first:offset lines))
    else (* XXX not thread-safe *)
      let kind_s = Str.matched_group 1 lines in
      let hash = Str.matched_group 2 lines in
      let name = Str.matched_group 3 lines in
      let scaff = match kind_s with
      |"100644 blob" -> PlainBlob hash
      |"100755 blob" -> ExeBlob hash
      |"040000 tree" -> TreeHash hash
      |_ -> assert false
      in (name, scaff)::(parse lines (Str.match_end ()))
  in parse lines 0

let tree_children, known_hashes =
  let children_cache = Hashtbl.create 16
  in let tree_children hash =
    try
      Hashtbl.find children_cache hash
    with Not_found ->
      let children = tree_children_uncached hash in
      Hashtbl.add children_cache hash children;
      children
  and known_hashes () =
    let acc = ref [] in
    Hashtbl.iter (fun k v -> acc := k::!acc) children_cache;
    !acc
  in tree_children, known_hashes

let tree_child hash child =
  List.assoc child (tree_children hash)

let depth_of_scaff = function
  |RootScaff -> 0
  |RefScaff _ -> 2
  |CommitHash _ -> 2
  |_ -> failwith "not implemented"

let scaffolding_child scaff child =
  match scaff with
  |RootScaff -> List.assoc child root_al
  |TreesScaff -> TreeHash child (* XXX should check for existence *)
  |RefsScaff -> let ref = reslash child in RefScaff ref
  |CommitsScaff -> CommitHash child
  |PlainBlob _ -> raise Not_found
  |ExeBlob _ -> raise Not_found
  |TreeHash hash -> tree_child hash child
  |RefScaff name ->
    if child = "current" then commit_symlink_of_ref name (depth_of_scaff scaff)
    (*else if child = "reflog" then ReflogScaff name*)
    else raise Not_found
  |CommitHash hash ->
    if child = "msg" then CommitMsg hash
    else if child = "worktree" then tree_symlink_of_commit hash (depth_of_scaff
    scaff)
    else if child = "parents" then CommitParents hash
    else raise Not_found
  |CommitMsg _ -> raise Not_found
  |CommitParents _ -> raise Not_found
  |Symlink _ -> raise Not_found
  |OtherHash _ -> raise Not_found

let list_children = function
  |RootScaff -> List.map fst root_al
  |TreesScaff -> (* Not complete, but we won't scan the whole repo here. *)
      known_hashes ()
  |RefsScaff -> let heads = wait_on_monad (repo#heads ()) in
   List.map (fun (n, h) -> slash_free (trim_endline n)) heads
  |RefScaff name -> [ "current"; (*"reflog";*) ]
  |CommitsScaff -> [] (* XXX *)
  |TreeHash hash -> List.map fst (tree_children hash)
  |CommitHash _ -> [ "msg"; "worktree"; "parents"; ]
  |PlainBlob _ -> raise Not_found
  |ExeBlob _ -> raise Not_found
  |OtherHash _ -> raise Not_found
  |CommitMsg _ -> raise Not_found
  |CommitParents _ -> raise Not_found
  |Symlink _ -> raise Not_found


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



let lookup_fh fh =
  Hashtbl.find fh_data fh

let lookup_and_cache path =
  try
    Hashtbl.find fh_by_name path
  with Not_found ->
    let scaff = lookup RootScaff path in
    prime_cache path scaff


let do_getattr path =
  try
    let fh, scaff = lookup_and_cache path in
    match scaff with
    |RootScaff -> dir_stats
    |TreesScaff -> dir_stats
    |RefsScaff -> dir_stats
    |CommitsScaff -> dir_stats
    |TreeHash _ -> dir_stats
    |CommitHash _ -> dir_stats
    |RefScaff _ -> dir_stats
    |CommitParents _ -> dir_stats
    |PlainBlob _ -> file_stats
    |ExeBlob _ -> exe_stats
    |OtherHash _ -> file_stats
    |CommitMsg _ -> file_stats
    |Symlink _ -> symlink_stats
    with Not_found ->
      raise (Unix_error (ENOENT, "stat", path))

let do_opendir path flags =
  (*prerr_endline ("Path is: " ^ path);*)
  try
    let fh, scaff = lookup_and_cache path in
    Some fh
  with Not_found ->
    raise (Unix_error (ENOENT, "opendir", path))

let do_readdir path fh =
  try
    let scaff = lookup_fh fh in
    "."::".."::(list_children scaff)
  with Not_found ->
    if true then assert false (* because opendir passed *)
    else raise (Unix_error (ENOENT, "readdir", path))

let do_readlink path =
  try
    let fh, scaff = lookup_and_cache path in
    match scaff with
    |Symlink target -> target
    |_ -> raise (Unix_error (EINVAL, "readlink (not a link)", path))
  with Not_found ->
    raise (Unix_error (ENOENT, "readlink", path))


let do_fopen path flags =
  try
    let fh, scaff = lookup_and_cache path in
    match scaff with
    |PlainBlob _ -> Some fh
    |ExeBlob _ -> Some fh
    |OtherHash _ -> Some fh
    |CommitMsg _ -> Some fh
    (* |Symlink _ -> () *) (* our symlinks all point to directories *)
    (* XXX Maybe introduce different symlinks for our hashlinks
     * and the symlinks git repos can contain. *)
    |_ -> raise (Unix_error (EINVAL, "fopen (not a file)", path))
  with Not_found ->
    raise (Unix_error (ENOENT, "fopen", path))

let do_read path buf ofs fh =
  try
    let scaff = lookup_fh fh in ignore scaff;
    if ofs > (Int64.of_int max_int) then 0
    else
      let ofs = Int64.to_int ofs in
      let len = min ((Array1.dim contents) - ofs) (Array1.dim buf) in
      (Array1.blit (Array1.sub contents ofs len) (Array1.sub buf 0 len);
      len)
  with Not_found -> raise (Unix_error (ENOENT, "read", path))

let _ =
  main Sys.argv
    {
      default_operations with
        getattr = do_getattr;
        opendir = do_opendir;
        readdir = do_readdir;
        readlink = do_readlink;
        fopen = do_fopen;
        read = do_read;
    }

