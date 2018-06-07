type t = {
  basePath: Path.t,
  lockfilePath: Path.t,
  tarballCachePath: Path.t,
  esyOpamOverride: checkout,
  opamRepository: checkout,
  npmRegistry: string,
}
and checkout =
  | Local(Path.t)
  | Remote(string, Path.t)
and checkoutCfg = [
  | `Local(Path.t)
  | `Remote(string)
  | `RemoteLocal(string, Path.t)
];

let resolvedPrefix = "esyi5-";

let configureCheckout = (~defaultRemote, ~defaultLocal) =>
  fun
  | Some(`RemoteLocal(remote, local)) => Remote(remote, local)
  | Some(`Remote(remote)) => Remote(remote, defaultLocal)
  | Some(`Local(local)) => Local(local)
  | None => Remote(defaultRemote, defaultLocal);

let make =
    (
      ~npmRegistry=?,
      ~cachePath=?,
      ~opamRepository=?,
      ~esyOpamOverride=?,
      basePath,
    ) =>
  RunAsync.Syntax.(
    {
      let%bind cachePath =
        RunAsync.ofRun(
          Run.Syntax.(
            switch (cachePath) {
            | Some(cachePath) => return(cachePath)
            | None =>
              let%bind userDir = Path.user();
              return(Path.(userDir / ".esy" / "esyi"));
            }
          ),
        );

      let tarballCachePath = Path.(cachePath / "tarballs");
      let%bind () = Fs.createDir(tarballCachePath);

      let opamRepository = {
        let defaultRemote = "https://github.com/ocaml/opam-repository";
        let defaultLocal = Path.(cachePath / "opam-repository");
        configureCheckout(~defaultLocal, ~defaultRemote, opamRepository);
      };

      let esyOpamOverride = {
        let defaultRemote = "https://github.com/esy-ocaml/esy-opam-override";
        let defaultLocal = Path.(cachePath / "esy-opam-override");
        configureCheckout(~defaultLocal, ~defaultRemote, esyOpamOverride);
      };

      let npmRegistry =
        Option.orDefault(~default="http://registry.npmjs.org/", npmRegistry);

      return({
        basePath,
        lockfilePath: Path.(basePath / "esyi.lock.json"),
        tarballCachePath,
        opamRepository,
        esyOpamOverride,
        npmRegistry,
      });
    }
  );
