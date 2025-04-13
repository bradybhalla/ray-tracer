open Math
open Utils
open Shape

type t = { shape : Shape.t; material : Material.t }

let get_intersection (prim : t) (ray : Ray.t) =
  let { shape; material } = prim in
  match shape with
  | Sphere { pos; radius } ->
      let a = Vec3.dot ray.dir ray.dir in
      let b = -2.0 *. Vec3.dot ray.dir (pos -@ ray.origin) in
      let c =
        Vec3.dot (pos -@ ray.origin) (pos -@ ray.origin) -. (radius *. radius)
      in
      let discriminant = (b *. b) -. (4.0 *. a *. c) in
      if discriminant < 0.0 then None
      else
        let t1 = (-.b +. sqrt discriminant) /. (2.0 *. a) in
        let t2 = (-.b -. sqrt discriminant) /. (2.0 *. a) in
        let t1, t2 = (min t1 t2, max t1 t2) in
        if t2 < 0.0 then
          (* both intersections are negative *)
          None
        else if t1 > 0.0 then
          (* both are positive (t1 is the min) *)
          Some
            Intersection.
              {
                time = t1;
                point = Ray.at ray t1;
                normal = Ray.at ray t1 -@ pos |> Vec3.normalize;
                material;
                flipped_refractive_index = 1.5;
              }
        else
          (* inside sphere (t2 is min positive), flip normal *)
          Some
            Intersection.
              {
                time = t2;
                point = Ray.at ray t2;
                normal = pos -@ Ray.at ray t2 |> Vec3.normalize;
                material;
                flipped_refractive_index = 1.0;
              }
  | Plane { normal; pos } ->
      let num = Vec3.dot (pos -@ ray.origin) normal in
      let denom = Vec3.dot ray.dir normal in
      if denom = 0.0 then
        (* ray parallel to plane *)
        None
      else
        let t = num /. denom in
        if t < 0.0 then
          (* intersection is negative *)
          None
        else
          let flip_normal = Vec3.dot ray.dir normal > 0.0 in
          Some
            {
              time = t;
              point = Ray.at ray t;
              normal = (if not flip_normal then normal else normal *@ -1.0);
              material;
              flipped_refractive_index = 1.0;
            }

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

let get_first_intersection (prims : t list) (ray : Ray.t) =
  prims |> List.filter_map (fun p -> get_intersection p ray) |> min_helper
