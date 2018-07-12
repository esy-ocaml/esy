module MakeFormula = Version.Formula.Make
module MakeConstraint = Version.Constraint.Make
module String = Astring.String

(** opam versions are Debian-style versions *)
module Version = struct
  type t = OpamPackage.Version.t

  let equal a b = OpamPackage.Version.compare a b = 0
  let compare = OpamPackage.Version.compare
  let show = OpamPackage.Version.to_string
  let pp fmt v = Fmt.pf fmt "opam:%s" (show v)
  let parse v = Ok (OpamPackage.Version.of_string v)
  let parseExn v = OpamPackage.Version.of_string v
  let prerelease _v = false
  let stripPrerelease v = v
  let toString = OpamPackage.Version.to_string
  let to_yojson v = `String (show v)
  let of_yojson = function
    | `String v -> parse v
    | _ -> Error "expected a string"

  let ofSemver v =
    let v = SemverVersion.Version.toString v in
    parse v
end

let caretRange v =
  match SemverVersion.Version.parse v with
  | Ok v ->
    let open Result.Syntax in
    let ve =
      if v.major = 0
      then {v with minor = v.minor + 1}
      else {v with major = v.major + 1}
    in
    let%bind v = Version.ofSemver v in
    let%bind ve = Version.ofSemver ve in
    Ok (v, ve)
  | Error _ -> Error ("^ cannot be applied to: " ^ v)

let tildaRange v =
  match SemverVersion.Version.parse v with
  | Ok v ->
    let open Result.Syntax in
    let ve = {v with minor = v.minor + 1} in
    let%bind v = Version.ofSemver v in
    let%bind ve = Version.ofSemver ve in
    Ok (v, ve)
  | Error _ -> Error ("~ cannot be applied to: " ^ v)

module Constraint = MakeConstraint(Version)

(**
 * Npm formulas over opam versions.
 *)
module Formula = struct

  include MakeFormula(Version)

  let any: DNF.t = [[Constraint.ANY]];

  module C = Constraint

  let parseRel text =
    let module String = Astring.String in
    let open Result.Syntax in
    match String.trim text with
    | "*"  | "" -> return [C.ANY]
    | text ->
      begin match text.[0], text.[1] with
      | '^', _ ->
        let v = String.Sub.(text |> v ~start:1 |> to_string) in
        let%bind v, ve = caretRange v in
        return [C.GTE v; C.LT ve]
      | '~', _ ->
        let v = String.Sub.(text |> v ~start:1 |> to_string) in
        let%bind v, ve = tildaRange v in
        return [C.GTE v; C.LT ve]
      | '=', _ ->
        let text = String.Sub.(text |> v ~start:1 |> to_string) in
        let%bind v = Version.parse text in
        return [C.EQ v]
      | '<', '=' ->
        let text = String.Sub.(text |> v ~start:2 |> to_string) in
        let%bind v = Version.parse text in
        return [C.LTE v]
      | '<', _ ->
        let text = String.Sub.(text |> v ~start:1 |> to_string) in
        let%bind v = Version.parse text in
        return [C.LT v]
      | '>', '=' ->
        let text = String.Sub.(text |> v ~start:2 |> to_string) in
        let%bind v = Version.parse text in
        return [C.GTE v]
      | '>', _ ->
        let text = String.Sub.(text |> v ~start:1 |> to_string) in
        let%bind v = Version.parse text in
        return [C.GT v]
      | _, _ ->
        let%bind v = Version.parse text in
        return [C.EQ v]
      end

  (* TODO: do not use failwith here *)
  let parse v =
    let parseSimple v =
      let parse v =
        let v = String.trim v in
        if v = ""
        then [C.ANY]
        else match parseRel v with
        | Ok v -> v
        | Error err -> failwith ("Error: " ^ err)
      in
      let (conjs) = Parse.conjunction ~parse v in
      let conjs =
        let f conjs c = conjs @ c in
        List.fold_left ~init:[] ~f conjs
      in
      let conjs = match conjs with | [] -> [C.ANY] | conjs -> conjs in
      conjs
    in
    Parse.disjunction ~parse:parseSimple v

  let%test_module "parse" = (module struct
    let v = Version.parseExn

    let parsesOk f e =
      let pf = parse f in
      if pf <> e
      then failwith ("Received: " ^ (DNF.show pf))
      else ()

    let%test_unit _ = parsesOk ">=1.7.0" ([[C.GTE (v "1.7.0")]])
    let%test_unit _ = parsesOk "*" ([[C.ANY]])
    let%test_unit _ = parsesOk "" ([[C.ANY]])

  end)

  let%test_module "matches" = (module struct
    let v = Version.parseExn
    let f = parse

    let%test _ = DNF.matches ~version:(v "1.8.0") (f ">=1.7.0")
    let%test _ = DNF.matches ~version:(v "0.3") (f "=0.3")
    let%test _ = DNF.matches ~version:(v "0.3") (f "0.3")

  end)


end
