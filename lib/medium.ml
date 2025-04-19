(* index of refraction *)
type t = float

let default = 1.0

type transition = In2Out | Out2In | Out2Out
type spec = { inside : t; outside : t }

let default_spec = { inside = 1.0; outside = 1.0 }

let get_incident spec = function
  | In2Out -> spec.inside
  | Out2In | Out2Out -> spec.outside

let get_transmitted spec = function
  | Out2In -> spec.inside
  | In2Out | Out2Out -> spec.outside
