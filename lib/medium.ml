
(* index of refraction *)
type t = float
let default = 1.0

type transition = { inside : t; outside : t }
let default_transition = {inside= 1.0; outside= 1.0}
