[@deriving of_yojson]
type t = {prefixPath: option(Path.t)};

let empty = {prefixPath: None};

let ofPath = path => {
  open RunAsync.Syntax;

  let normalizePath = p =>
    if (Path.isAbs(p)) {
      p;
    } else {
      Path.(normalize(path /\/ p));
    };

  let ofFile = filename => {
    let* data = Fs.readFile(filename);
    let* json =
      switch (Json.parse(data)) {
      | Ok(json) => return(json)
      | Error(err) =>
        errorf(
          "expected %a to be a JSON file but got error: %a",
          Path.pp,
          filename,
          Run.ppError,
          err,
        )
      };

    let* rc = RunAsync.ofStringError(of_yojson(json));
    let rc = {prefixPath: Option.map(~f=normalizePath, rc.prefixPath)};
    return(rc);
  };

  let filename = Path.(path / ".esyrc");
  let filenameInHome = Path.(Path.homePath() / ".esyrc");

  if%bind (Fs.exists(filename)) {
    ofFile(filename);
  } else {
    if%bind (Fs.exists(filenameInHome)) {
      ofFile(filenameInHome);
    } else {
      return(empty);
    };
  };
};
