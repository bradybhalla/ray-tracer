open Utils
open Math

type t = {
  shape : Shape.t;
  material : Material.t;
  light : Light.geometric_light option;
  medium : Medium.spec;
}

type intersection = {
  time : float;
  si : shape_intersection;
  wo : Vec3.t; (* vector back to ray origin *)
  prim : t;
}

let get_intersection ~(ray : Ray.t) ~(primitive : t) =
  let shape_int = Shape.intersect ~shape:primitive.shape ~ray in
  match shape_int with
  | None -> None
  | Some (t, si) ->
      Some { time = t; si; prim = primitive; wo = ray.dir *@ -1.0 }
