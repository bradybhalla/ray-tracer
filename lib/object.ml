open Math

type t = Sphere of { pos : Vec3.t; radius : float } | Collection of t list

let rec ray_intersection (ray : Ray.t) (obj : t) =
  match obj with
  | Sphere { pos; radius } ->
      (* TODO: currently stub code that is not very reliable  *)
      (* also should eventually output an actual intersection type  *)
      let oc = Vec3.(ray.origin -@ pos) in
      let a = Vec3.dot ray.dir ray.dir in
      let b = 2.0 *. Vec3.dot oc ray.dir in
      let c = Vec3.dot oc oc -. (radius *. radius) in
      let discriminant = (b *. b) -. (4.0 *. a *. c) in
      if discriminant < 0.0 then None
      else
        let t1 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
        let t2 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
        Some (min t1 t2)
  | Collection l ->
      let min_intersection =
        List.filter_map (ray_intersection ray) l |> List.fold_left min max_float
      in
      if min_intersection = max_float then None else Some min_intersection
