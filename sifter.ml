(* vim: set tw=0 sw=2 ts=2 et : *)

(* I dislike opens, but struct labels would be ugly otherwise *)
open Unix.LargeFile


(* Run a command, return stdout data as a string *)
(* Going through the shell is evil/ugly,
 * but I didn't find a non system()-based api *)
(* Unlike a shell backtick, doesn't remove trailing newlines *)
(* XXX Can't have the exit status AFAICT (didn't read the source) *)
let backtick shell_cmd =
  prerr_endline (Printf.sprintf "Command “%S”" shell_cmd); flush_all ();
  let out_pipe = BatUnix.open_process_in shell_cmd in
  let r = BatIO.read_all out_pipe in
  let status = BatUnix.close_process_in out_pipe in
  if status <> Unix.WEXITED 0
  then failwith "Non-zero exit status"
  else r

(* Run a command, read the output into a BigArray.Array1.
 *)
let subprocess_read_bigarray shell_cmd offset big_array =
  prerr_endline (Printf.sprintf "Command “%S”" shell_cmd); flush_all ();
  let out_pipe = BatUnix.open_process_in shell_cmd in
  let out_fd = BatUnix.descr_of_input out_pipe in
  (* Can't seek a pipe. Read and ignore. *)
  (* XXX lossy int64 conversion *)
  ignore (BatIO.really_nread out_pipe (Int64.to_int offset));
  (* Returns how much was read, may raise. *)
  let r = Unix_util.read out_fd big_array in
  let status = BatUnix.close_process_in out_pipe in
  if status <> Unix.WEXITED 0
  then failwith "Non-zero exit status"
  else r


let trim_endline str =
  (* XXX not what the spec says, this trims both ends *)
  BatString.trim str

let git_dir_r = ref "" (* XXX Ugly global *)

let backtick_git cmd =
  let cmd_str = String.concat " " ("git"::"--git-dir"::!git_dir_r::cmd) in
  backtick cmd_str

let subprocess_read_bigarray_git cmd offset big_array =
  let cmd_str = String.concat " " ("git"::"--git-dir"::!git_dir_r::cmd) in
  subprocess_read_bigarray cmd_str offset big_array


let dir_stats = Unix.LargeFile.stat "." (* XXX *)
let file_stats = { dir_stats with
  st_nlink = 1;
  st_kind = Unix.S_REG;
  st_perm = 0o400;
  (* /proc uses zero, it works.
   * /sys uses 4k.
   * zero doesn't work with fuse, at least high-level fuse.
   * (unless it's cat acting up, but proc indicates otherwise.
   * strace cat with 0 size someday)
   *)
  (*st_size = Int64.zero;*)
  st_size = Int64.of_int 4096;
  }
let exe_stats = { file_stats with
  st_perm = 0o500;
  }
let symlink_stats = { file_stats with
  st_kind = Unix.S_LNK;
  }


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

let rec canonical = function
  |RootScaff -> "."
  |TreesScaff -> "trees"
  |RefsScaff -> "refs"
  |CommitsScaff -> "commits"
  |TreeHash hash -> (canonical TreesScaff) ^ "/" ^ hash
  |CommitHash hash -> (canonical CommitsScaff) ^ "/" ^ hash
  |_ -> failwith "Not implemented"

let rec parents_depth depth =
  if depth = 0 then ""
  else "../" ^ parents_depth (depth - 1)

let symlink_to_scaff scaff depth =
  let path = canonical scaff in
  let to_root = parents_depth depth in
  Symlink (to_root ^ path)

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

let hashtable_keys htbl =
  let acc = ref [] in
  Hashtbl.iter (fun k v -> acc := k::!acc) htbl;
  !acc

let known_commit_hashes_ = ref BatSet.StringSet.empty

let known_commit_hashes () =
  BatSet.StringSet.elements !known_commit_hashes_

let commit_of_ref ref =
  let r = trim_endline (backtick_git [ "show-ref"; "--hash"; "--verify"; "--"; ref ]) in
  known_commit_hashes_ := BatSet.StringSet.add r !known_commit_hashes_;
  r

let tree_of_commit_with_prefix hash prefix =
  (* prefix should be empty or a relative path with no initial slash
   * and no . or .. *)
  trim_endline (backtick_git [ "rev-parse"; "--revs-only"; "--no-flags";
  "--verify"; "--quiet"; hash ^ "^{tree}" ^ ":" ^ prefix ])

let ref_names () =
  (**
   * These backslashes are the reason system() is evil and kills kittens.
   *
   * Because BatUnix calls system, we have shell code potentially everywhere.
   *
   * This result shouldn't be cached, unlike most of the git data model
   * it's not a functional data structure and may mutate.
   *)
  List.map trim_endline (
    BatString.nsplit (
      backtick_git [ "for-each-ref"; "--format"; "%\\(refname\\)"; ]
      )
    "\n"
    )

let tree_of_commit hash =
  tree_of_commit_with_prefix hash ""

let commit_symlink_of_ref ref depth =
  let scaff = CommitHash (commit_of_ref ref) in
  symlink_to_scaff scaff depth

let tree_symlink_of_commit hash depth =
  let scaff = TreeHash (tree_of_commit hash) in
  symlink_to_scaff scaff depth

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

let tree_children, known_tree_hashes =
  let children_cache = Hashtbl.create 16
  in let tree_children hash =
    try
      Hashtbl.find children_cache hash
    with Not_found ->
      let children = tree_children_uncached hash in
      Hashtbl.add children_cache hash children;
      children
  and known_tree_hashes () =
    hashtable_keys children_cache
  in tree_children, known_tree_hashes

let tree_child hash child =
  List.assoc child (tree_children hash)
let tree_children_names hash =
  List.map fst (tree_children hash)

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
  |RefScaff name when child = "current" ->
      commit_symlink_of_ref name (depth_of_scaff scaff)
  (*|RefScaff name when child = "reflog" -> ReflogScaff name*)
  |RefScaff name -> raise Not_found
  |CommitHash hash when child = "msg" -> CommitMsg hash
  (*|CommitHash hash when child = "parents" -> CommitParents hash*) (* XXX *)
  |CommitHash hash when child = "worktree" ->
      tree_symlink_of_commit hash (depth_of_scaff scaff)
  |CommitHash _ -> raise Not_found
  |CommitMsg _ -> raise Not_found
  |CommitParents _ -> raise Not_found
  |Symlink _ -> raise Not_found
  |OtherHash _ -> raise Not_found

let list_children = function
  |RootScaff -> List.map fst root_al
  |TreesScaff -> (* Not complete, but we won't scan the whole repo here. *)
      known_tree_hashes ()
  |CommitsScaff -> known_commit_hashes () (* Not complete either. *)
  |RefsScaff ->
   List.map slash_free (ref_names ())
  |RefScaff name -> [ "current"; (*"reflog";*) ]
  |TreeHash hash -> tree_children_names hash
  |CommitHash _ -> [ "msg"; "worktree"; (*"parents";*) ]
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


let blob_size_uncached hash =
  int_of_string (trim_endline (backtick_git [ "cat-file"; "-s"; hash; ]))

let blob_size =
  let cache = Hashtbl.create 16
  in let blob_size hash =
    try
      Hashtbl.find cache hash
    with Not_found ->
      let r = blob_size_uncached hash in
      Hashtbl.add cache hash r;
      r
  in blob_size

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

    |ExeBlob _ -> exe_stats

    |PlainBlob _ -> file_stats
    |OtherHash _ -> file_stats
    |CommitMsg _ -> file_stats

    |Symlink _ -> symlink_stats
    with Not_found ->
      raise (Unix.Unix_error (Unix.ENOENT, "stat", path))

let do_opendir path flags =
  (*prerr_endline ("Path is: " ^ path);*)
  try
    let fh, scaff = lookup_and_cache path in
    let r = Some fh in
    match scaff with
    |RootScaff -> r
    |TreesScaff -> r
    |RefsScaff -> r
    |CommitsScaff -> r
    |TreeHash _ -> r
    |CommitHash _ -> r
    |RefScaff _ -> r
    |CommitParents _ -> failwith "Not implemented"
    |_ -> raise (Unix.Unix_error (Unix.EINVAL, "opendir (not a directory)", path))
  with Not_found ->
    raise (Unix.Unix_error (Unix.ENOENT, "opendir", path))

let do_readdir path fh =
  try
    let scaff = lookup_fh fh in
    "."::".."::(list_children scaff)
  with Not_found ->
    prerr_endline (Printf.sprintf "Can't readdir “%S”" path); flush_all ();
    if true then assert false (* because opendir passed *)
    else raise (Unix.Unix_error (Unix.ENOENT, "readdir", path))

let do_readlink path =
  try
    let fh, scaff = lookup_and_cache path in
    match scaff with
    |Symlink target -> target
    |_ -> raise (Unix.Unix_error (Unix.EINVAL, "readlink (not a link)", path))
  with Not_found ->
    raise (Unix.Unix_error (Unix.ENOENT, "readlink", path))


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
    |_ -> raise (Unix.Unix_error (Unix.EINVAL, "fopen (not a file)", path))
  with Not_found ->
    raise (Unix.Unix_error (Unix.ENOENT, "fopen", path))

(* Read file data into a Bigarray.Array1.
 *
 * libfuse-ocaml takes a string, making it simpler than ocamlfuse.
 *)
let do_read path buf ofs fh =
  try
    let scaff = lookup_fh fh in ignore scaff;
    let hash = match scaff with
    |PlainBlob hash -> hash
    |ExeBlob hash -> hash
    |_ -> failwith "Not a blob"
    in
    let did_read_len =
      subprocess_read_bigarray_git [ "cat-file"; "blob"; hash; ] ofs buf in
    did_read_len
  with Not_found ->
    raise (Unix.Unix_error (Unix.ENOENT, "read", path))

let _ =
  git_dir_r := trim_endline (backtick "git rev-parse --git-dir");
  Fuse.main Sys.argv
    {
      Fuse.default_operations with
        Fuse.getattr = do_getattr;
        Fuse.opendir = do_opendir;
        Fuse.readdir = do_readdir;
        Fuse.readlink = do_readlink;
        Fuse.fopen = do_fopen;
        Fuse.read = do_read;
    }

