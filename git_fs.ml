(* vim: set tw=0 sw=2 ts=2 et : *)

(* Isn't there a simpler syntax? *)
module UL = struct
  include Unix.LargeFile
end

let require_normal_exit out_pipe =
  let status = BatUnix.close_process_in out_pipe in
  if status <> Unix.WEXITED 0
  then failwith "Non-zero exit status"

(* Run a command, return stdout data as a string *)
(* Going through the shell is evil/ugly,
   but I didn't find a non system()-based api *)
(* Unlike a shell backtick, doesn't remove trailing newlines *)
let backtick shell_cmd =
  prerr_endline (Printf.sprintf "Command %S" shell_cmd);
  let out_pipe = BatUnix.open_process_in shell_cmd in
  let r = BatIO.read_all out_pipe in
  require_normal_exit out_pipe;
  r

(* Run a command, read the output into a BigArray.Array1. *)
let subprocess_read_bigarray shell_cmd offset big_array =
  prerr_endline (Printf.sprintf "Command %S" shell_cmd);
  let out_pipe = BatUnix.open_process_in shell_cmd in
  let out_fd = BatUnix.descr_of_input out_pipe in
  (* Can't seek a pipe. Read and ignore. *)
  (* XXX lossy int64 conversion *)
  ignore (BatIO.really_nread out_pipe (Int64.to_int offset));
  (* Returns how much was read, may raise. *)
  let r = Unix_util.read out_fd big_array in
  require_normal_exit out_pipe;
  r


let trim_endline str =
  (* XXX not what the spec says, this trims both ends *)
  BatString.trim str

let git_dir_lazy = lazy (
  let r = trim_endline (backtick "git rev-parse --git-dir")
  in if r <> "" then r else failwith "Git directory not found."
)

let backtick_git cmd =
  let lazy git_dir = git_dir_lazy in
  let cmd_str = String.concat " " ("git"::"--git-dir"::git_dir::cmd) in
  backtick cmd_str

let subprocess_read_bigarray_git cmd offset big_array =
  let lazy git_dir = git_dir_lazy in
  let cmd_str = String.concat " " ("git"::"--git-dir"::git_dir::cmd) in
  subprocess_read_bigarray cmd_str offset big_array


let dir_stats = Unix.LargeFile.stat "." (* XXX *)
let file_stats = { dir_stats with
  UL.st_nlink = 1;
  UL.st_kind = Unix.S_REG;
  UL.st_perm = 0o400;
  (* /proc uses zero, it works.
   * /sys uses 4k.
   * zero doesn't work with fuse, at least high-level fuse.
   * (unless it's cat acting up, but proc indicates otherwise.
   * strace cat with 0 size someday)
   *)
  (*UL.st_size = Int64.zero;*)
  UL.st_size = Int64.of_int 4096;
  }
let exe_stats = { file_stats with
  UL.st_perm = 0o500;
  }
let symlink_stats = { file_stats with
  UL.st_kind = Unix.S_LNK;
  }


type hash = string

type ref_tree_i = (string * ref_tree) list
and ref_tree =
  |RefTreeInternalNode of ref_tree_i
  |RefTreeLeaf

type scaffolding =
  |RootScaff
  |TreesScaff
  (* prefix, and a subtree we haven't traversed yet *)
  |RefsScaff of string * ref_tree_i
  |CommitsScaff
  |RefScaff of string
  |Symlink of string
  |PlainBlob of hash
  |ExeBlob of hash
  |TreeHash of hash
  |CommitHash of hash
  |CommitMsg of hash
  |CommitParents of hash
  (*|OtherHash of hash (* gitlink, etc *)*)

let rec canonical = function
  |RootScaff -> "."
  |TreesScaff -> "trees"
  |RefsScaff (prefix, subtree) ->
      if prefix = "" then "refs" else "refs" ^ "/" ^ prefix
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

let commit_parents hash =
  let r = BatString.nsplit (backtick_git
    [ "log"; "-n1"; "--format=format:'%P'"; hash; ]) " "
  in List.iter (fun h ->
    known_commit_hashes_ := BatSet.StringSet.add h !known_commit_hashes_)
    r;
  r

let commit_parents_pretty_names hash =
  match commit_parents hash with
  |[] -> []
  |p0::tl -> (hash ^ "^")::(BatList.mapi (fun i h ->
      hash ^ "^" ^ (string_of_int (i+1)))
      tl)

let parent_symlink merged parent_id depth =
  let fail () = failwith (Printf.sprintf
        "%S has incorrect syntax for a parent of %S" parent_id merged) in
  if not (BatString.starts_with parent_id (merged ^ "^")) then fail ();
  let suffix = BatString.tail parent_id 41 in
  prerr_endline suffix;
  let parent_idx = if suffix = "" then 0 else int_of_string suffix in
  let hash = List.nth (commit_parents merged) parent_idx in
  symlink_to_scaff (CommitHash hash) depth

let ref_names () =
  (**
   * These backslashes are the reason system() is evil and kills kittens.
   *
   * Because BatUnix calls system, we have shell code potentially everywhere.
   *
   * This result shouldn't be cached, unlike most of the git data model
   * it's not a functional data structure and may mutate.
   *)
  List.filter (fun s -> s <> "") (
    BatString.nsplit (
      backtick_git [ "for-each-ref"; "--format"; "%\\(refname\\)"; ]
      )
    "\n"
    )

let rec ref_tree_add tree path =
  (* this traversal relies on the sort order *)
  match tree, path with
  (* git maintains that invariant for us anyway. *)
  |_, [] -> failwith "Can't make an internal node into a leaf"
  |((name, RefTreeInternalNode grand_children)::children_tl), name_::tl
  when name = name_ ->
    (name, RefTreeInternalNode (ref_tree_add grand_children tl)
      )::children_tl
  |children, name::[] -> (* sort order *)
      (name, RefTreeLeaf)::children
  |children, name::tl -> (* sort order *)
      (name, RefTreeInternalNode (ref_tree_add [] tl))::children

let ref_tree () =
  let refs = ref_names () in
  let tree = ref [] in
  List.iter (fun refname ->
    let refpath = BatString.nsplit refname "/" in
    tree := ref_tree_add !tree refpath;
    )
    refs;
  !tree

(* association list for the fs root *)
(* takes unit, lazy would also work *)
let root_al () = [
  "trees", TreesScaff;
  "refs", RefsScaff ("", ref_tree ());
  "commits", CommitsScaff;
  "heads", Symlink "refs/refs/heads";
  "remotes", Symlink "refs/refs/remotes";
  ]


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

let scaffolding_child scaff child =
  match scaff with
  |RootScaff -> List.assoc child (root_al ())
  |TreesScaff -> TreeHash child (* XXX should check for existence *)
  |RefsScaff (prefix, children) -> (
    let pf2 = if prefix = "" then child else prefix ^ "/" ^ child in
      match List.assoc child children with
      |RefTreeLeaf -> RefScaff pf2
      |RefTreeInternalNode children -> RefsScaff (pf2, children)
      )
  |CommitsScaff -> CommitHash child
  |PlainBlob _ -> raise Not_found
  |ExeBlob _ -> raise Not_found
  |TreeHash hash -> tree_child hash child
  |RefScaff name when child = "current" ->
      commit_symlink_of_ref name (1 + List.length (BatString.nsplit name "/"))
  |RefScaff name when child = "worktree" ->
      Symlink "current/worktree"
  (*|RefScaff name when child = "reflog" -> ReflogScaff name*)
  |RefScaff name -> raise Not_found
  |CommitHash hash when child = "msg" -> CommitMsg hash
  |CommitHash hash when child = "parents" -> CommitParents hash
  |CommitHash hash when child = "worktree" ->
      tree_symlink_of_commit hash 2
  |CommitHash _ -> raise Not_found
  |CommitMsg _ -> raise Not_found
  |CommitParents hash ->
      (* here, child confusingly means parent in git semantics *)
      parent_symlink hash child 3
  |Symlink _ -> raise Not_found

let list_children = function
  |RootScaff -> List.map fst (root_al ())
  |TreesScaff -> (* Not complete, but we won't scan the whole repo here. *)
      known_tree_hashes ()
  |CommitsScaff -> known_commit_hashes () (* Not complete either. *)
  |RefsScaff (prefix, children) ->
      List.map fst children
  |RefScaff name -> [ "current"; "worktree"; (*"reflog";*) ]
  |TreeHash hash -> tree_children_names hash
  |CommitHash _ -> [ "msg"; "worktree"; "parents"; ]
  |PlainBlob _ -> failwith "Plain file"
  |ExeBlob _ -> failwith "Plain file"
  |CommitMsg _ -> failwith "Plain file"
  |CommitParents hash -> commit_parents_pretty_names hash
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
    |RefsScaff _ -> dir_stats
    |CommitsScaff -> dir_stats
    |TreeHash _ -> dir_stats
    |CommitHash _ -> dir_stats
    |RefScaff _ -> dir_stats
    |CommitParents _ -> dir_stats

    |ExeBlob _ -> exe_stats

    |PlainBlob _ -> file_stats
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
    |RefsScaff _ -> r
    |CommitsScaff -> r
    |TreeHash _ -> r
    |CommitHash _ -> r
    |RefScaff _ -> r
    |CommitParents _ -> r
    |_ -> raise (Unix.Unix_error (Unix.EINVAL, "opendir (not a directory)", path))
  with Not_found ->
    raise (Unix.Unix_error (Unix.ENOENT, "opendir", path))

let do_readdir path fh =
  try
    let scaff = lookup_fh fh in
    "."::".."::(list_children scaff)
  with Not_found ->
    prerr_endline (Printf.sprintf "Can't readdir “%S”" path);
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
    |CommitMsg _ -> Some fh
    (* |Symlink _ -> () *) (* our symlinks all point to directories *)
    (* XXX Maybe introduce different symlinks for our hashlinks
     * and the symlinks git repos can contain. *)
    |_ -> raise (Unix.Unix_error (Unix.EINVAL, "fopen (not a file)", path))
  with Not_found ->
    raise (Unix.Unix_error (Unix.ENOENT, "fopen", path))

(* Read file data into a Bigarray.Array1.

   libfuse-ocaml takes a string, making it simpler than ocamlfuse.
   *)
let do_read path buf ofs fh =
  try
    let scaff = lookup_fh fh in ignore scaff;
    match scaff with
    |PlainBlob hash ->
        subprocess_read_bigarray_git [ "cat-file"; "blob"; hash; ] ofs buf
    |ExeBlob hash ->
        subprocess_read_bigarray_git [ "cat-file"; "blob"; hash; ] ofs buf
    |CommitMsg hash ->
        (* Not exactly the raw message, but there's no api to get it.
         * %s and %b don't go far. There's rewrapping and stuff. *)
        subprocess_read_bigarray_git [ "log"; "--max-count=1"; hash; ] ofs buf
    |_ -> assert false (* we filtered at fopen time *)
    with Not_found ->
      raise (Unix.Unix_error (Unix.ENOENT, "read", path))

let fuse_ops = {
      Fuse.default_operations with
        Fuse.getattr = do_getattr;
        Fuse.opendir = do_opendir;
        Fuse.readdir = do_readdir;
        Fuse.readlink = do_readlink;
        Fuse.fopen = do_fopen;
        Fuse.read = do_read;
    }

let mountpoint_lazy =
  lazy (let lazy git_dir = git_dir_lazy in git_dir ^ "/fs")

let abspath path =
  if not (Filename.is_relative path) then path
  else (Unix.getcwd ()) ^ "/" ^ path

let cmd_mount () =
  let lazy mountpoint = mountpoint_lazy in
  let lazy git_dir = git_dir_lazy in
  (* fuse doesn't guess the subtype if we give it fsname *)
  let subtype = Filename.basename Sys.argv.(0) in
  try Unix.mkdir mountpoint 0o0755
  with Unix.Unix_error(Unix.EEXIST, _, _) -> ();
  prerr_endline (Printf.sprintf "Mounting on %S" mountpoint);
  let fuse_args = [|
    subtype; "-f"; "-oro";
    "-osubtype=" ^ subtype;
    "-ofsname=" ^ (abspath git_dir); (* XXX needs ","-quoting *)
    mountpoint;
    |] in
  Fuse.main fuse_args fuse_ops

let cmd_umount () =
  let lazy mountpoint = mountpoint_lazy in
  try
    ignore (backtick ("fusermount -u -- " ^ mountpoint))
  with
    Failure "Non-zero exit status" -> ()

let cmd_show_mountpoint () =
  let lazy mountpoint = mountpoint_lazy in
  print_endline mountpoint

let usage () =
  prerr_endline "Usage: git fs [mount|umount|show-mountpoint|help]"

let cmd_help = usage

let cmd_fuse_help () =
  Fuse.main [| Sys.argv.(0); "--help"; |] fuse_ops

let _ =
  match Sys.argv with
  |[| _; |] -> cmd_mount ()
  |[| _; "mount"; |] -> cmd_mount ()
  |[| _; "umount"; |] -> cmd_umount ()
  |[| _; "show-mountpoint"; |] -> cmd_show_mountpoint ()
  |[| _; "help"; |] -> cmd_help ()
  |[| _; "fuse-help"; |] -> cmd_fuse_help () (* For developer use *)
  |_ -> begin usage (); exit 2; end

