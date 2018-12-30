open EsyPackageConfig

let traverse pkg =
  PackageId.Set.elements pkg.Package.dependencies

let traverseWithDevDependencies pkg =
  let dependencies =
    PackageId.Set.union
      pkg.Package.dependencies
      pkg.Package.devDependencies
  in
  PackageId.Set.elements dependencies

include Graph.Make(struct
  include Package
  let traverse = traverse
  let id pkg = pkg.id
  module Id = PackageId
end)

let findByPath p solution =
  let open Option.Syntax in
  let f _id pkg =
    match pkg.Package.source with
    | Link {path; manifest = None; kind = _; } ->
      let path = DistPath.(path / "package.json") in
      DistPath.compare path p = 0
    | Link {path; manifest = Some filename; kind = _;} ->
      let path = DistPath.(path / ManifestSpec.show filename) in
      DistPath.compare path p = 0
    | _ -> false
  in
  let%map _id, pkg = findBy f solution in
  pkg

let findByName name solution =
  let open Option.Syntax in
  let f _id pkg =
    String.compare pkg.Package.name name = 0
  in
  let%map _id, pkg = findBy f solution in
  pkg

let findByNameVersion name version solution =
  let open Option.Syntax in
  let compare = [%derive.ord: string * Version.t] in
  let f _id pkg =
    compare (pkg.Package.name, pkg.Package.version) (name, version) = 0
  in
  let%map _id, pkg = findBy f solution in
  pkg
