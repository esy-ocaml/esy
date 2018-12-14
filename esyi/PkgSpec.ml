type t =
  | Root
  | ByName of string
  | ByNameVersion of (string * EsyInstall.Version.t)
  | ById of EsyInstall.PackageId.t

let pp fmt = function
  | Root -> Fmt.unit "root" fmt ()
  | ByName name -> Fmt.string fmt name
  | ByNameVersion (name, version) -> Fmt.pf fmt "%s@%a" name EsyInstall.Version.pp version
  | ById id -> EsyInstall.PackageId.pp fmt id

let parse =
  let open Result.Syntax in
  function
  | "root" -> return Root
  | v ->
    let split = Astring.String.cut ~sep:"@" in
    let rec parsename v =
      match split v with
      | Some ("", v) ->
        let name, rest = parsename v in
        "@" ^ name, rest
      | Some (name, rest) ->
        name, Some rest
      | None -> v, None
    in
    match parsename v with
    | name, Some ""
    | name, None -> return (ByName name)
    | name, Some rest ->
      begin match split rest with
      | Some _ ->
        let%bind id = EsyInstall.PackageId.parse v in
        return (ById id)
      | None ->
        let%bind version = EsyInstall.Version.parse rest in
        return (ByNameVersion (name, version))
      end
