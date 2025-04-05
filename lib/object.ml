open Math

module Shape = struct
  type t = Sphere of { pos : Vec3.t; radius : float }
end

module Material = struct
  type t = { color : Vec3.t }
end

module Intersection = struct
  type t = {
    time : float;
    point : Vec3.t;
    normal : Vec3.t;
    material : Material.t;
  }
end

module Primitive = struct
  type t = { shape : Shape.t; material : Material.t }
end

let ray_intersection (ray : Ray.t) (prim : Primitive.t) =
  let Primitive.{ shape; material } = prim in
  match shape with
  | Shape.Sphere { pos; radius } ->
      (* TODO: currently stub code that is not very reliable  *)
      (* also should eventually output an actual intersection type  *)
      let oc = ray.origin -@ pos in
      let a = Vec3.dot ray.dir ray.dir in
      let b = 2.0 *. Vec3.dot oc ray.dir in
      let c = Vec3.dot oc oc -. (radius *. radius) in
      let discriminant = (b *. b) -. (4.0 *. a *. c) in
      if discriminant < 0.0 then None
      else
        let t1 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
        let t2 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
        let t = min t1 t2 in
        Some
          Intersection.
            {
              time = t;
              point = Ray.at ray t;
              normal = Ray.at ray t -@ pos |> Vec3.normalize;
              material;
            }

let first_ray_intersection (ray : Ray.t) (prims : Primitive.t list) =
  let min_helper (ints : Intersection.t list) : Intersection.t option =
    match ints with
    | [] -> None
    | h :: t ->
        let res =
          List.fold_left
            Intersection.(fun i i' -> if i.time < i'.time then i else i')
            h t
        in
        Some res
  in
  prims |> List.filter_map (ray_intersection ray) |> min_helper
