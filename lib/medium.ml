open Math

(* index of refraction *)
type t = float

let default = 1.0

type transition = In2Out | Out2In | Out2Out
type spec = { inside : t; outside : t }

let default_spec = { inside = 1.0; outside = 1.0 }

let get_incident ~spec ~transition = match transition with
  | In2Out -> spec.inside
  | Out2In | Out2Out -> spec.outside

let get_transmitted ~spec ~transition = match transition with
  | Out2In -> spec.inside
  | In2Out | Out2Out -> spec.outside

let ray_side_normal ~transition ~outward_normal = match transition with
  | In2Out -> outward_normal *@ (-1.0)
  | Out2Out -> outward_normal (* out2out transition means ray-side normal was returned *)
  | Out2In -> outward_normal
