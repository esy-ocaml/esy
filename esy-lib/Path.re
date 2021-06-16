type t = Fpath.t;
type ext = Fpath.ext;

module Set = Fpath.Set;

let v = Fpath.v;
// in Fpath / is an alias to add_seg which fails if !is_seg
let (/) = (path, string) =>
  Fpath.(/\/)(path, Stdlib.Result.get_ok(string |> Fpath.of_string));
let (/\/) = Fpath.(/\/);

let dirSep = Fpath.dir_sep;

let hasExt = Fpath.has_ext;
let addExt = Fpath.add_ext;
let remExt = Fpath.rem_ext;
let getExt = Fpath.get_ext;

let remExtOfFilename = (~multi=?, filename) => {
  let p = v(filename);
  let p = remExt(~multi?, p);
  Fpath.to_string(p);
};

let addSeg = Fpath.add_seg;

let ofString = v => {
  let v = Fpath.of_string(v);
  (v: result(t, [ | `Msg(string)]) :> result(t, [> | `Msg(string)]));
};

let sexp_of_t = p => Sexplib0.Sexp.Atom(Fpath.to_string(p));

let isAbs = Fpath.is_abs;
let isPrefix = Fpath.is_prefix;
let remPrefix = Fpath.rem_prefix;

let homePath = () =>
  Fpath.v(
    switch (Sys.getenv_opt("HOME"), System.Platform.host) {
    | (Some(dir), _) => dir
    | (None, System.Platform.Windows) => Sys.getenv("USERPROFILE")
    | (None, _) => failwith("Could not find HOME dir")
    },
  );
let dataPath = () =>
  Fpath.v(
    switch (System.Platform.host) {
    | System.Platform.Windows => Sys.getenv("LOCALAPPDATA")
    | _ => Sys.getenv("HOME")
    },
  );

let currentPath = () =>
  switch (Bos.OS.Dir.current()) {
  | Ok(path) => path
  | Error(`Msg(msg)) =>
    failwith("Unable to determine current working dir: " ++ msg)
  };

let exePath' = () => {
  switch (Sys.getenv_opt("_")) {
  | Some(p) => p
  | None =>
    switch (System.Platform.host) {
    | Linux => Unix.readlink("/proc/self/exe")
    | Darwin
    | Cygwin
    | Windows
    | Unix
    | Unknown => Sys.argv[0]
    }
  // TODO cross-platform solution to getting full path of the current executable.
  // Linux has /proc/self/exe. Macos ?? Windows GetModuleFileName()
  // https://stackoverflow.com/a/1024937
  };
};

let exePath = () => v @@ exePath'();

let relativize = Fpath.relativize;
let parent = Fpath.parent;
let basename = Fpath.basename;
let append = Fpath.append;

let tryRelativize = (~root, p) =>
  switch (relativize(~root, p)) {
  | Some(p) => p
  | None => p
  };

let tryRelativizeToCurrent = p => {
  let root = currentPath();
  if (Fpath.equal(root, p)) {
    Fpath.v(".");
  } else {
    tryRelativize(~root, p);
  };
};

let backSlashRegex = Str.regexp("\\\\");

let normalizePathSepOfFilename = p =>
  Str.global_replace(backSlashRegex, "/", p);

let remEmptySeg = Fpath.rem_empty_seg;
let normalize = Fpath.normalize;
let normalizeAndRemoveEmptySeg = p =>
  Fpath.rem_empty_seg(Fpath.normalize(p));

/* COMPARABLE */

let compare = Fpath.compare;

/* PRINTABLE */

let show = Fpath.to_string;
let pp = Fpath.pp;

let showNormalized = p => {
  let p = show(p);
  normalizePathSepOfFilename(p);
};

let showPretty = p => {
  let p =
    switch (remPrefix(homePath(), p)) {
    | Some(p) => Fpath.append(Fpath.v("~"), p)
    | None => p
    };
  let p = tryRelativizeToCurrent(p);
  show(p);
};

let ppPretty = (fmt, p) => Fmt.string(fmt, showPretty(p));

/* JSONABLE */

let of_yojson = (json: Yojson.Safe.t) =>
  switch (json) {
  | `String(v) =>
    switch (Fpath.of_string(v)) {
    | Ok(v) => Ok(v)
    | Error(`Msg(msg)) => Error(msg)
    }
  | _ => Error("invalid path")
  };

let to_yojson = (path: t) => `String(show(path));

let safeSeg = {
  let replaceAt = Str.regexp("@");
  let replaceUnderscore = Str.regexp("_+");
  let replaceSlash = Str.regexp("\\/");
  let replaceDash = Str.regexp("\\-");
  let replaceColon = Str.regexp(":");
  let make = (name: string) =>
    name
    |> String.lowercase_ascii
    |> Str.global_replace(replaceAt, "")
    |> Str.global_replace(replaceUnderscore, "__")
    |> Str.global_replace(replaceSlash, "__s__")
    |> Str.global_replace(replaceColon, "__c__")
    |> Str.global_replace(replaceDash, "_");
  make;
};

let safePath = {
  let replaceSlash = Str.regexp("\\/");
  let replaceColon = Str.regexp(":");
  let make = name =>
    name
    |> Str.global_replace(replaceSlash, "__s__")
    |> Str.global_replace(replaceColon, "__c__");
  make;
};
