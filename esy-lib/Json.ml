type t = Yojson.Safe.json

type 'a encoder = 'a -> t
type 'a decoder = t -> ('a, string) result

let to_yojson x = x
let of_yojson x = Ok x

let parseJsonWith parser json =
  Run.ofStringError (parser json)

let parseStringWith parser data =
  let json = Yojson.Safe.from_string data in
  parseJsonWith parser json

module Parse = struct

  let string (json : t) =
    match json with
    | `String v -> Ok v
    | _ -> Error "expected string"

  let assoc (json : t) =
    match json with
    | `Assoc v -> Ok v
    | _ -> Error "expected object"

  let field ~name (json : t) =
    match json with
    | `Assoc items ->
      begin match List.find_opt (fun (k, _v) -> k = name) items with
      | Some (_, v) -> Ok v
      | None -> Error ("no such field: " ^ name)
      end
    | _ -> Error "expected object"

  let list ?(errorMsg="expected an array") value (json : t) =
    match json with
    | `List (items : t list) ->
      let c acc v = match acc, (value v) with
        | Ok acc, Ok v -> Ok (v::acc)
        | Ok _, Error err -> Error err
        | err, _ -> err
      in begin
      match List.fold_left c (Ok []) items with
      | Ok items -> Ok (List.rev items)
      | error -> error
      end
    | _ -> Error errorMsg

  let stringMap ?(errorMsg= "expected an object") value (json : t) =
    match json with
    | `Assoc items ->
      let c acc (k, v) = match acc, k, (value v) with
        | Ok acc, k, Ok v -> Ok (StringMap.add k v acc)
        | Ok _, _, Error err -> Error err
        | err, _, _ -> err
      in
      List.fold_left c (Ok StringMap.empty) items
    | _ -> Error errorMsg

  let cmd ?(errorMsg="expected a string or an array of strings") (json : t) =
    match json with
    | `List cmd ->
      begin match list string (`List cmd) with
      | Ok cmd -> Ok (Cmd.ofList cmd)
      | Error _ -> Error errorMsg
      end
    | `String cmd ->
      begin match ShellSplit.split cmd with
      | Ok argv -> Ok (Cmd.ofList argv)
      | Error _ -> Error errorMsg
      end
    | _ -> Error errorMsg
end
