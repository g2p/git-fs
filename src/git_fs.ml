(* vim: set tw=0 sw=2 ts=2 et : *)

(* Isn't there a simpler syntax? *)
module UL = struct
  include Unix.LargeFile
end

(* a |> b |> c is equivalent to c (b a).
 * I haven't found Haskell's ($) operator yet. *)
let (|>) = BatPervasives.(|>)

module Subprocess = struct
  (* http://caml.inria.fr/cgi-bin/viewcvs.cgi/ocaml/trunk/otherlibs/unix/unix.ml?view=markup *)

  (* Consider getting a patch that adds an alternative open_proc implementation.
   * Could use labels, pick execve / execvp / execvpe /execv,
   * even take a
   *   type ShellCommand of string | ExecCommand of string array
   *)

  open Unix

  type popen_process =
    | Process of in_channel * out_channel
    | Process_in of in_channel
    | Process_out of out_channel
    | Process_full of in_channel * out_channel * in_channel

  let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

  let open_proc cmd proc input output toclose =
    let _ = List.iter set_close_on_exec toclose in
    match fork () with
    | 0 ->
        if input <> stdin then begin dup2 input stdin; close input end;
        if output <> stdout then begin dup2 output stdout; close output end;
        begin try execvp cmd.(0) cmd
        with _ -> exit 127
        end;
    | id -> Hashtbl.add popen_processes proc id

  let open_process_in cmd =
    let (in_read, in_write) = pipe() in
    let inchan = in_channel_of_descr in_read in
    open_proc cmd (Process_in inchan) stdin in_write [in_read];
    close in_write;
    inchan

  let find_proc_id fun_name proc =
    try
      let pid = Hashtbl.find popen_processes proc in
      Hashtbl.remove popen_processes proc;
      pid
    with Not_found ->
      raise(Unix_error(EBADF, fun_name, ""))

  let rec waitpid_non_intr pid =
    try waitpid [] pid
    with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

  let close_process_in inchan =
    let pid = find_proc_id "close_process_in" (Process_in inchan) in
    close_in inchan;
    snd(waitpid_non_intr pid)

end

module SubprocessWithBatIO = struct
  module Wrapped_in = BatInnerWeaktbl.Make(BatInnerIO.Input) (*input  -> in_channel *)
  let wrapped_in    = Wrapped_in.create 16

  let open_process_in cmd =
    let inchan = Subprocess.open_process_in cmd in
    (* close the fd ourselves (cleanup=false) or close_process_in breaks *)
    let r = BatUnix.input_of_descr ~autoclose:false ~cleanup:false (
      Unix.descr_of_in_channel inchan) in
    Wrapped_in.add wrapped_in r inchan;
    r

  let descr_of_input = BatUnix.descr_of_input

  let close_process_in cin =
    let inchan = Wrapped_in.find wrapped_in cin in
    Wrapped_in.remove wrapped_in cin;
    try Subprocess.close_process_in inchan
    with Not_found ->
      raise (Unix.Unix_error(Unix.EBADF, "close_process_in", ""))

end

let require_normal_exit out_pipe =
  let status = SubprocessWithBatIO.close_process_in out_pipe in
  if status <> Unix.WEXITED 0
  then failwith "Non-zero exit status"

(* Run a command, return stdout data as a string *)
(* Unlike a shell backtick, doesn't remove trailing newlines *)
let backtick cmd =
  prerr_endline (Printf.sprintf "Command %S" (BatString.join " " cmd));
  let out_pipe = SubprocessWithBatIO.open_process_in (Array.of_list cmd) in
  let r = BatIO.read_all out_pipe in
  require_normal_exit out_pipe;
  r

(* Run a command, read the output into a BigArray.Array1. *)
let subprocess_read_bigarray cmd offset big_array =
  prerr_endline (Printf.sprintf "Command %S" (BatString.join " " cmd));
  let out_pipe = SubprocessWithBatIO.open_process_in (Array.of_list cmd) in
  let out_fd = SubprocessWithBatIO.descr_of_input out_pipe in
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
  let r = trim_endline (backtick ["git"; "rev-parse"; "--git-dir"; ])
  in if r <> "" then r else failwith "Git directory not found."
)

let backtick_git cmd =
  let lazy git_dir = git_dir_lazy in
  let cmd = "git"::"--git-dir"::git_dir::cmd in
  backtick cmd

let subprocess_read_bigarray_git cmd offset big_array =
  let lazy git_dir = git_dir_lazy in
  let cmd = "git"::"--git-dir"::git_dir::cmd in
  subprocess_read_bigarray cmd offset big_array

let describe_tag hash =
  () (* git cat-file tag demo-tag *)

let symlink_target hash =
  (* XXX may be abs or rel, and may go outside the worktree *)
  backtick_git [ "cat-file"; "blob"; hash; ]


let dir_stats = Unix.LargeFile.stat "." (* XXX *)
let file_stats = { dir_stats with
  UL.st_kind = Unix.S_REG;
  UL.st_nlink = 1;
  UL.st_perm = 0o400;
  (* /proc uses zero, it works.
   * /sys uses 4k.
   * zero doesn't work with fuse, at least high-level fuse.
   * (unless it's cat acting up, but proc indicates otherwise.
   * strace cat with 0 size someday)
   *)
  (*UL.st_size = Int64.zero;*)
  UL.st_size = Int64.of_int 4096; (* XXX *)
  }
let blob_stats size is_exe = { file_stats with
  UL.st_size = size;
  UL.st_perm = if is_exe then 0o500 else 0o400;
  }

let symlink_stats = { dir_stats with
  UL.st_kind = Unix.S_LNK;
  UL.st_nlink = 1;
  UL.st_perm = 0o400;
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
  |ReflogScaff of string
  |FsSymlink of string
  |WorktreeSymlink of hash
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
  FsSymlink (to_root ^ path)

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
    [ "log"; "-n1"; "--format=format:%P"; hash; ]) " "
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
  if not (BatString.starts_with parent_id (merged ^ "^"))
  then failwith (Printf.sprintf
        "%S has incorrect syntax for a parent of %S" parent_id merged);
  let suffix = BatString.tail parent_id 41 in
  prerr_endline suffix;
  let parent_idx = if suffix = "" then 0 else int_of_string suffix in
  let hash = List.nth (commit_parents merged) parent_idx in
  symlink_to_scaff (CommitHash hash) depth

let ref_names () =
  (**
   * This result shouldn't be cached, unlike most of the git data model
   * it's not a functional data structure and may mutate.
   *)
  List.filter (fun s -> s <> "") (
    BatString.nsplit (
      backtick_git [ "for-each-ref"; "--format"; "%(refname)"; ]
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

let ref_tree_cache = ref None

let ref_tree_uncached () =
  let refs = ref_names () in
  let tree = ref [] in
  List.iter (fun refname ->
    let refpath = BatString.nsplit refname "/" in
    tree := ref_tree_add !tree refpath;
    )
    refs;
    ref_tree_cache := Some (!tree, Unix.time ());
  !tree

let ref_tree () =
  match !ref_tree_cache with
  |None -> ref_tree_uncached ()
  |Some (cached, tstamp) when tstamp > Unix.time () +. 300. ->
      ref_tree_uncached () (* every 300s = 5mn *)
  |Some (cached, tstamp) -> cached


let reflog_entries name =
  let r = List.filter (fun s -> s <> "") (
    BatString.nsplit (backtick_git [ "rev-list"; "-g"; name; ]) "\n")
  in List.iter (fun h ->
    known_commit_hashes_ := BatSet.StringSet.add h !known_commit_hashes_)
    r;
  r

let reflog_entries_pretty_names name =
  let entries = reflog_entries name in
  let n = List.length entries in
  let width = n - 1 |> float_of_int |> log10 |> ceil |> int_of_float in
  BatList.mapi (fun i h ->
    "@{" ^ (Printf.sprintf "%0*d" width i) ^ "}") entries


let reflog_regexp = Str.regexp "^\\(.*\\)@{\\([0-9]+\\)}$"

let reflog_entry name child depth =
  let fail () = failwith (Printf.sprintf
        "%S has incorrect syntax for a reflog entry of %S" child name) in
  if not (Str.string_match reflog_regexp child 0 ) then fail ();
  if Str.matched_group 1 child <> "" then fail ();
  let hash = trim_endline (backtick_git [ "rev-parse";
    "--revs-only"; "--no-flags"; "--verify"; "--quiet"; name ^ child ])
  in
    symlink_to_scaff (CommitHash hash) depth


(* association list for the fs root *)
(* takes unit, lazy would also work *)
let root_al () = [
  "trees", TreesScaff;
  "refs", RefsScaff ("", ref_tree ());
  "commits", CommitsScaff;
  "heads", FsSymlink "refs/refs/heads";
  "remotes", FsSymlink "refs/refs/remotes";
  "tags", FsSymlink "refs/refs/tags";
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

let ls_tree_regexp = Str.regexp "\\(100644 blob\\|100755 blob\\|120000 blob\\|040000 tree\\) \\([0-9a-f]+\\)\t\\([^\000]+\\)\000"

let tree_children_uncached hash =
  let lines = backtick_git [ "ls-tree"; "-z"; "--"; hash; ] in
  let rec parse lines offset =
    if String.length lines = offset then []
    else if not (Str.string_match ls_tree_regexp lines offset)
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
      |"120000 blob" -> WorktreeSymlink hash
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
      FsSymlink "current/worktree"
  |RefScaff name when child = "reflog" -> ReflogScaff name
  |RefScaff name -> raise Not_found
  |ReflogScaff name -> reflog_entry name child (2 + List.length (BatString.nsplit name "/"))
  |CommitHash hash when child = "msg" -> CommitMsg hash
  |CommitHash hash when child = "parents" -> CommitParents hash
  |CommitHash hash when child = "worktree" ->
      tree_symlink_of_commit hash 2
  |CommitHash _ -> raise Not_found
  |CommitMsg _ -> raise Not_found
  |CommitParents hash ->
      (* here, child confusingly means parent in git semantics *)
      parent_symlink hash child 3
  |FsSymlink _ -> raise Not_found
  |WorktreeSymlink _ -> raise Not_found

let list_children = function
  |RootScaff -> List.map fst (root_al ())
  |TreesScaff -> (* Not complete, but we won't scan the whole repo here. *)
      known_tree_hashes ()
  |CommitsScaff -> known_commit_hashes () (* Not complete either. *)
  |RefsScaff (prefix, children) ->
      List.map fst children
  |RefScaff name -> [ "current"; "worktree"; "reflog"; ]
  |ReflogScaff name -> reflog_entries_pretty_names name
  |TreeHash hash -> tree_children_names hash
  |CommitHash _ -> [ "msg"; "worktree"; "parents"; ]
  |PlainBlob _ -> failwith "Plain file"
  |ExeBlob _ -> failwith "Plain file"
  |CommitMsg _ -> failwith "Plain file"
  |CommitParents hash -> commit_parents_pretty_names hash
  |FsSymlink _ -> failwith "Symlink"
  |WorktreeSymlink _ -> failwith "Symlink" (* XXX I'm not sure *)


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
  Int64.of_string (trim_endline (backtick_git [ "cat-file"; "-s"; hash; ]))

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

let blob_stats_by_hash hash is_exe =
  blob_stats (blob_size hash) is_exe


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
    |ReflogScaff _ -> dir_stats
    |CommitParents _ -> dir_stats

    |ExeBlob hash -> blob_stats_by_hash hash true
    |PlainBlob hash -> blob_stats_by_hash hash false
    |CommitMsg _ -> file_stats

    |FsSymlink _ -> symlink_stats
    |WorktreeSymlink _ -> symlink_stats
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
    |ReflogScaff _ -> r
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
    |FsSymlink target -> target
    |WorktreeSymlink hash ->
        symlink_target hash (* XXX: these are allowed to go outside the tree *)
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
    (* |FsSymlink _ -> () *) (* our symlinks all point to directories *)
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
    subtype; "-f";
    "-o"; "ro";
    "-osubtype=" ^ subtype;
    "-ofsname=" ^ (abspath git_dir); (* XXX needs ","-quoting *)
    mountpoint;
    |] in
  Fuse.main fuse_args fuse_ops

let cmd_umount () =
  let lazy mountpoint = mountpoint_lazy in
  try
    ignore (backtick ["fusermount"; "-u"; "--"; mountpoint])
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
  |[| _ |] -> cmd_mount ()
  |[| _; "mount" |] -> cmd_mount ()
  |[| _; "umount" |] -> cmd_umount ()
  |[| _; "show-mountpoint" |] -> cmd_show_mountpoint ()
  |[| _; "help" |] -> cmd_help ()
  |[| _; "fuse-help" |] -> cmd_fuse_help () (* For developer use *)
  |_ -> begin usage (); exit 2; end

