(* test.ml: example for libfuse-ocaml
 * Copyright (C) 2009 Goswin von Brederlow
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * Under Debian a copy can be found in /usr/share/common-licenses/LGPL-3.
 *)

type entry =
    Dir of (string * Fuse.inode) list
  | File of string

let root_stat = 
  {
    Fuse.st_dev = 0;
    Fuse.st_ino = Fuse.root_inode;
    Fuse.st_mode = Fuse.s_IFDIR + 0o777;
    Fuse.st_nlink = 2;
    Fuse.st_uid = 0;
    Fuse.st_gid = 0;
    Fuse.st_rdev = 0;
    Fuse.st_size = Int64.of_int(4096);
    Fuse.st_blksize = Int64.of_int(4096);
    Fuse.st_blocks = Int64.of_int(1);
    Fuse.st_atime = 0.0;
    Fuse.st_mtime = 0.0;
    Fuse.st_ctime = 0.0;
  }

let hello_stat = 
  {
    Fuse.st_dev = 0;
    Fuse.st_ino = Int64.of_int 2;
    Fuse.st_mode = Fuse.s_IFREG + 0o644;
    Fuse.st_nlink = 1;
    Fuse.st_uid = 0;
    Fuse.st_gid = 0;
    Fuse.st_rdev = 0;
    Fuse.st_size = Int64.of_int(13);
    Fuse.st_blksize = Int64.of_int(4096);
    Fuse.st_blocks = Int64.of_int(1);
    Fuse.st_atime = 0.0;
    Fuse.st_mtime = 0.0;
    Fuse.st_ctime = 0.0;
  }

let last_inode = ref 2

let new_stats is_file =
  incr last_inode;
  {
    Fuse.st_dev = 0;
    Fuse.st_ino = Int64.of_int !last_inode;
    Fuse.st_mode =
      if is_file
      then Fuse.s_IFREG + 0o644
      else Fuse.s_IFDIR + 0o755;
    Fuse.st_nlink = if is_file then 1 else 2;
    Fuse.st_uid = 0;
    Fuse.st_gid = 0;
    Fuse.st_rdev = 0;
    Fuse.st_size = Int64.of_int(0);
    Fuse.st_blksize = Int64.of_int(4096);
    Fuse.st_blocks = Int64.of_int(1);
    Fuse.st_atime = 0.0;
    Fuse.st_mtime = 0.0;
    Fuse.st_ctime = 0.0;
  }

let fs =
  let fs = Hashtbl.create 2
  in
    Hashtbl.add fs Fuse.root_inode (root_stat, Dir [(".", Fuse.root_inode); ("..", Fuse.root_inode); ("hello", Int64.of_int 2)]);
    Hashtbl.add fs (Int64.of_int 2) (hello_stat, File "Hello World!\n");
    fs

let fs_init () =
  Printf.printf "### fs_init\n";
  flush_all ();
  ()

let fs_destroy () =
  Printf.printf "### fs_destroy\n";
  flush_all ();
  ()

let stats inode =
  let (stats, _) = Hashtbl.find fs inode
  in
    stats

let fs_lookup inode name =
  Printf.printf "### fs_lookup\n";
  flush_all ();
  try
    let (_, entry) = Hashtbl.find fs inode
    in
      match entry with
	  Dir(list) ->
	    let ino = List.assoc name list
	    in
	      {
		Fuse.e_inode = ino;
		Fuse.e_generation = Int64.one;
		Fuse.e_stats = stats ino;
		Fuse.e_stats_timeout = 1.0;
		Fuse.e_entry_timeout = 1.0;
	      }
	| File(_) -> raise(Fuse.Error(Fuse.ENOTDIR))
  with Not_found -> raise(Fuse.Error(Fuse.ENOENT))

let fs_getattr inode =
  Printf.printf "### fs_getattr %Lu\n" inode;
  flush_all ();
  try
    (stats inode, 1.0)
  with Not_found -> raise(Fuse.Error(Fuse.ENOENT))

let fs_read inode size off file_info =
  Printf.printf "### fs_read %Lu offset = %Lu\n" inode off;
  flush_all ();
  try
    let (_, entry) = Hashtbl.find fs inode
    in
      match entry with
	  Dir(list) -> raise(Fuse.Error(Fuse.EISDIR))
	| File(data) ->
	    let off = Int64.to_int off in
	    let len = String.length data in
	    let len = max 0 (min (len - off) size)
	    in
	      if off > String.length data
	      then ""
	      else String.sub data 0 len
  with Not_found -> raise(Fuse.Error(Fuse.ENOENT))

let fs_write inode buf off file_info =
  Printf.printf "### fs_write %Lu offset = %Lu\n" inode off;
  flush_all ();
  try
    let (stats, entry) = Hashtbl.find fs inode
    in
      match entry with
	  Dir(list) -> raise(Fuse.Error(Fuse.EISDIR))
	| File(data) ->
	    let off = Int64.to_int off in
	    let file_len = String.length data in
	    let buf_len = String.length buf in
	    let data =
	      if off + buf_len > file_len
	      then begin
		let stats = { stats with
				Fuse.st_size = Int64.of_int (off + buf_len);
			    } in
		let str = String.create (off + buf_len)
		in
		  String.blit data file_len str 0 file_len;
		  Hashtbl.replace fs inode (stats, File(str));
		  str
	      end
	      else data
	    in
	      String.blit buf 0 data off buf_len;
	      buf_len
  with Not_found -> raise(Fuse.Error(Fuse.ENOENT))

let fs_readdir inode off file_info =
  Printf.printf "### fs_readdir %Lu offset = %Lu\n" inode off;
  flush_all ();
  try
    let (_, entry) = Hashtbl.find fs inode
    in
      match entry with
	  Dir(list) ->
	    let rec loop n = function
		[] -> raise(Fuse.Error(Fuse.SUCCESS))
	      | (name, inode)::xs when n = Int64.zero -> (name, stats inode, Int64.add off Int64.one)
	      | x::xs -> loop (Int64.sub n Int64.one) xs
	    in
	      loop off list
	| File(_) -> raise(Fuse.Error(Fuse.ENOTDIR))
  with Not_found -> raise(Fuse.Error(Fuse.ENOENT))

let fs_mknod parent name mode dev ctx =
  Printf.printf "### fs_mknod %Lu %s\n" parent name;
  flush_all ();
  try
    let (stats, entry) = Hashtbl.find fs parent
    in
      match entry with
	  Dir(list) ->
	    let new_stats = new_stats true in
	    let list = (name, new_stats.Fuse.st_ino)::list in
	    let stats = {stats with Fuse.st_nlink = stats.Fuse.st_nlink + 1 }
	    in
	      Hashtbl.replace fs parent (stats, Dir(list));
	      Hashtbl.add fs new_stats.Fuse.st_ino (new_stats, File(""));
	      {
		Fuse.e_inode = new_stats.Fuse.st_ino;
		Fuse.e_generation = Int64.one;
		Fuse.e_stats = new_stats;
		Fuse.e_stats_timeout = 1.0;
		Fuse.e_entry_timeout = 1.0;
	      }
	| File(_) -> raise(Fuse.Error(Fuse.ENOTDIR))
  with Not_found -> raise(Fuse.Error(Fuse.ENOENT))

let fs_ops = {
  Fuse.default_ops with
    Fuse.init    = Some fs_init;
    Fuse.destroy = Some fs_destroy;
    Fuse.lookup  = Some fs_lookup;
    Fuse.getattr = Some fs_getattr;
    Fuse.read    = Some fs_read;
    Fuse.write   = Some fs_write;
    Fuse.readdir = Some fs_readdir;
    Fuse.mknod   = Some fs_mknod;
}

let main args =
  Printf.printf "Fs V0.0\n";
  flush_all ();
  let filesystem =
    Fuse.make
      "tmp"
      ["-d";
       "-o"; "max_read=131072";
       "-o"; "max_write=131072";
       (*"-o"; "allow_other";*)
       "-o"; "default_permissions"]
      fs_ops
  in
  let rec loop () =
    Fuse.process filesystem;
    loop ()
  in
    loop ()

let _ =
  main Sys.argv
