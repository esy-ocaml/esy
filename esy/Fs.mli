
val readFile : Path.t -> string RunAsync.t

val readJsonFile : Path.t -> Yojson.Safe.json RunAsync.t

val openFile : mode:Lwt_unix.open_flag list -> perm:int -> Path.t -> Lwt_unix.file_descr RunAsync.t

val exists : Path.t -> bool RunAsync.t

val unlink : Path.t -> unit RunAsync.t

val stat : Path.t -> Unix.stats RunAsync.t

val createDirectory : Path.t -> unit RunAsync.t

val chmod : int -> Path.t -> unit RunAsync.t

val fold :
  ?skipTraverse : (Path.t -> bool)
  -> f : ('a -> Path.t -> Unix.stats -> 'a Lwt.t)
  -> init : 'a
  -> Path.t
  -> 'a RunAsync.t

val copyFile : origPath:Path.t -> destPath:Path.t -> unit RunAsync.t
val copyPath : origPath:Path.t -> destPath:Path.t -> unit RunAsync.t

val rmPath : Path.t -> [`Removed | `NoSuchPath] RunAsync.t

val withTempDir : ?tempDir:string -> (Path.t -> 'a Lwt.t) -> 'a Lwt.t
val withTempFile : string -> (Path.t -> 'a Lwt.t) -> 'a Lwt.t
