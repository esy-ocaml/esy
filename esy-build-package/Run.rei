/*

	A little DSL to script commands.

 */

type err('b) =
  [> | `Msg(string) | `CommandError(Cmd.t, Bos.OS.Cmd.status)] as 'b;

/** Computation which might succeed or fail. */
type t('v, 'e) = result('v, err('e));


/* Convenience */

let return : 'v => t('v, _);
let error : string => t(_, _);
let errorf : format4('a, Format.formatter, unit, t('v, 'e)) => 'a
let bind : (~f: 'a => result('b, 'c), result('a, 'c)) => result('b, 'c)
let coerceFromMsgOnly : result('a, [ `Msg(string) ]) => t('a, _);
let coerceFromClosed : result('a, [ `Msg(string) | `CommandError(Cmd.t, Bos.OS.Cmd.status) ]) => t('a, _);

let ok : t(unit, _);

module Let_syntax: {
  let bind: (~f: 'v1 => t('v2, 'err), t('v1, 'err)) => t('v2, 'err);
}


/* Path operations */

let v : string => EsyLib.Path.t;
let (/) : EsyLib.Path.t => string => EsyLib.Path.t;


/* Filesystem operations: common. */

/** Check if path exists. */
let exists : EsyLib.Path.t => t(bool, _);

/** Remove path, if it's a dir then remove it recursively. */
let rm : EsyLib.Path.t => t(unit, _);

/** Move. */
let mv : (~force: bool=?, EsyLib.Path.t, EsyLib.Path.t) => t(unit, _);

/** Resolve path using realpath. */
let realpath : EsyLib.Path.t => t(EsyLib.Path.t, _);

/** Get path stats. */
let stat : EsyLib.Path.t => t(Unix.stats, _);

/** Get path stats or return unix error. */
let statOrError : Fpath.t => result(Unix.stats, (Unix.error, string, string));

/** Get path stats (including info on symlinks). */
let lstat : EsyLib.Path.t => t(Unix.stats, _);

/** Get path stats or return unix error. */
let lstatOrError : Fpath.t => result(Unix.stats, (Unix.error, string, string));

/** Perform operation with a different working directory. */
let withCwd : (EsyLib.Path.t, ~f: unit => t('a, 'e)) => t('a, 'e);

/** Traverse path. */
let traverse : (
    EsyLib.Path.t,
    (EsyLib.Path.t, Unix.stats) => t(unit, 'e)
  ) => t(unit, 'e)


/* Filesystem operations: files. */

/** Read file into strinlg. */
let read : (EsyLib.Path.t) => t(string, _);

/** Write data into file */
let write : (~perm: int=?, ~data: string, EsyLib.Path.t) => t(unit, _);

/** Create temporary file with data. */
let createTmpFile : string => t(EsyLib.Path.t, _);


/* Filesystem operations: directories. */

/** List directory and return a list of relative paths. */
let ls : EsyLib.Path.t => t(list(EsyLib.Path.t), _);

/** Create a directory. */
let mkdir : EsyLib.Path.t => t(unit, _);

/** Copy contents of a directory to the destination directory. */
let copyContents : (~from: Fpath.t, ~ignore: list(string)=?, Fpath.t) => t(unit, _);


/* Filesystem operations: links. */

/** Create a  hard link. */
let link : (~force: bool=?, ~target: EsyLib.Path.t, EsyLib.Path.t) => t(unit, _);

/** Create a symlink. */
let symlink : (~force: bool=?, ~target: EsyLib.Path.t, EsyLib.Path.t) => t(unit, _);

/** Read symlink's target. */
let readlink : EsyLib.Path.t => t(EsyLib.Path.t, _);
