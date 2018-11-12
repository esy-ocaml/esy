include Path

let ofPath p = (normalizeAndRemoveEmptySeg p)

let toPath base p = normalizeAndRemoveEmptySeg (base // p)

let rebase ~base p = normalizeAndRemoveEmptySeg (base // p)

let render path = normalizePathSlashes (show path)

let show = render
let showPretty path = Path.(normalizePathSlashes (showPretty path))

let to_yojson path = `String (render path)

let sexp_of_t path = Sexplib0.Sexp.Atom (render path)
