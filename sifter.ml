open Unix
open LargeFile
open Bigarray
open Fuse

let default_stats = LargeFile.stat "."
let fname = "hello"
let name = "/" ^ fname
let contents : Fuse.buffer = Array1.of_array Bigarray.char Bigarray.c_layout 
  [|'H';'e';'l';'l';'o';' ';'w';'o';'r';'l';'d';'!'|]

let do_getattr path = 
  if path = "/" then default_stats
  else if path = name then 
    { default_stats with 
      st_nlink = 1;
      st_kind = S_REG;
      st_perm = 0o444;
      st_size = Int64.of_int (Array1.dim contents) }
  else raise (Unix_error (ENOENT,"stat",path))

let do_readdir path _ =
  if path = "/" then [".";"..";fname]
  else raise (Unix_error (ENOENT,"readdir",path))

let do_fopen path flags =
  if path = name then None
  else raise (Unix_error (ENOENT,"open",path))

let do_read path buf ofs _ =
  if path = name then
    if ofs > (Int64.of_int max_int) then 0    
    else
      let ofs = Int64.to_int ofs in
      let len = min ((Array1.dim contents) - ofs) (Array1.dim buf) in
	(Array1.blit (Array1.sub contents ofs len) (Array1.sub buf 0 len);
	 len)
  else raise (Unix_error (ENOENT,"read",path))
	   
let _ =
  main Sys.argv 
    { 
      default_operations with 
	getattr = do_getattr;
	readdir = do_readdir;
	fopen = do_fopen;
	read = do_read;
    }
