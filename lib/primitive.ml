open Utils

type t = {
  shape : Shape.t;
  material : Material.t;
  light : Light.geometric_light option;
  medium : Medium.spec;
}

type intersection = {
  time : float;
  si : shape_intersection;
  prim : t;
}

let get_intersection (ray : Ray.t) (prim : t) =
  let shape_int = Shape.intersect prim.shape ray in
  match shape_int with
  | None -> None
  | Some (t, si) -> Some { time = t; si; prim }
