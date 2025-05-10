type t = float
type direction = In2Out | Out2In | Out2Out
type transition = { inside : t; outside : t }

let default = 1.0
let default_transition = { inside = 1.0; outside = 1.0 }

let get_incident m = function
  | In2Out -> m.inside
  | Out2In | Out2Out -> m.outside

let get_transmitted m = function
  | Out2In -> m.inside
  | In2Out | Out2Out -> m.outside
