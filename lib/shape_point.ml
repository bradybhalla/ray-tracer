open Math
open Utils

type t = Vec3.t
type params = t

let create (point : params) : t = point

let intersect _ _ = None
let transform point tr = Transform.point tr point
let sample_point point = point
