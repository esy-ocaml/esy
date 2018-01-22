open Std

type binding = {
    name : string;
    value : string;
    origin : Package.t option;
  }
  [@@deriving show]

let renderStringWithConfig (cfg : Config.t) value =
  let lookup = function
  | "store" -> Some (Path.to_string cfg.storePath)
  | "localStore" -> Some (Path.to_string cfg.localStorePath)
  | "sandbox" -> Some (Path.to_string cfg.sandboxPath)
  | _ -> None
  in
  Run.liftOfBosError (EsyBuildPackage.PathSyntax.render lookup value)

(**
 * Render environment to a string.
 *)
let renderToShellSource
    ?(header="# Environment")
    (cfg : Config.t)
    (bindings : binding list) =
  let open Run.Syntax in
  let emptyLines = function
    | [] -> true
    | _ -> false
  in
  let f (lines, prevOrigin) ({ name; value; origin } : binding) =
    let lines = if prevOrigin <> origin || emptyLines lines then
      let header = match origin with
      | Some origin -> Printf.sprintf "\n#\n# Package %s@%s\n#" origin.name origin.version
      | None -> "\n#\n# Built-in\n#"
      in header::lines
    else
      lines
    in
    let%bind value = renderStringWithConfig cfg value in
    let line = Printf.sprintf "export %s=\"%s\"" name value in
    Ok (line::lines, origin)
  in
  let%bind lines, _ = Run.foldLeft ~f ~init:([], None) bindings in
  return (header ^ "\n" ^ (lines |> List.rev |> String.concat "\n"))

module Value = struct

  (*
   * Environment with values with no references to other environment variables.
   *)
  type t = string Astring.String.map

  module M = Astring.String.Map

  let find = M.find_opt

  let ofBindings ?(init : t = M.empty) (bindings : binding list) =
    let f env binding =
      let scope name = M.find name env in
      match ShellParamExpansion.render ~scope binding.value with
      | Ok value -> Ok (M.add binding.name value env)
      | Error err -> Error err
    in
    Result.listFoldLeft ~f ~init bindings

  let bindToConfig cfg env =
    let f k v = function
      | Ok env ->
        let open Run.Syntax in
        let%bind v = renderStringWithConfig cfg v in
        Ok (M.add k v env)
      | err -> err
    in
    M.fold f env (Ok M.empty)

  let to_yojson env =
    let f k v items = (k, `String v)::items in
    let items = M.fold f env [] in
    `Assoc items

end

(**
 * A closed environment (which doesn't have references outside of own values).
 *)
module Closed : sig

  type t

  val bindings : t -> binding list
  val value : t -> Value.t

  val ofBindings : binding list -> t Run.t

end = struct

  type t = (Value.t * binding list)

  let bindings (_, bindings) = bindings
  let value (value, _) = value

  let ofBindings bindings =
    let open Run.Syntax in
    let%bind value = Value.ofBindings bindings in
    Ok (value, bindings)
end

module PathLike = struct

  let make (name : string) (value : string list) =
    let sep = match System.host, name with
      | System.Cygwin, "OCAMLPATH" -> ";"
      | _ -> ":"
    in
    value |> String.concat sep

end
