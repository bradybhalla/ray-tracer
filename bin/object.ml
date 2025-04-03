open Ray_tracer.Math

type t = {pos: Vec3.t; radius: float}

let create ~pos ~radius = {pos; radius}

let ray_intersection (ray: Ray.t) (sphere: t) =
  (* TODO: currently stub code that is not very reliable  *)
  (* also should eventually output an actual intersection type  *)
  let oc = Vec3.(ray.origin -@ sphere.pos) in
  let a = Vec3.dot ray.dir ray.dir in
  let b = 2.0 *. Vec3.dot oc ray.dir in
  let c = Vec3.dot oc oc -. (sphere.radius *. sphere.radius) in
  let discriminant = b *. b -. 4.0 *. a *. c in
  if discriminant < 0.0 then None
  else
    let t1 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
    let t2 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
    Some (min t1 t2)
