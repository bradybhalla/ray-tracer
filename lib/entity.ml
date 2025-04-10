open Math

module Light = struct
  type t = Point of { pos : Vec3.t; power : float }
end

module Shape = struct
  type t =
    | Sphere of { pos : Vec3.t; radius : float }
    | Plane of { normal : Vec3.t; pos : Vec3.t }
end

module Material = struct
  type phong_props = {
    ambient : Vec3.t;
    diffuse : Vec3.t;
    specular : Vec3.t;
    shininess : float;
  }

  type t = Phong of phong_props | Reflective | Light

  let create = function
    | `Red ->
        Phong
          {
            ambient = Vec3.create 0.1 0.0 0.0;
            diffuse = Vec3.create 1.0 0.0 0.0;
            specular = Vec3.create 0.1 0.1 0.1;
            shininess = 10.0;
          }
    | `Green ->
        Phong
          {
            ambient = Vec3.create 0.0 0.0 0.0;
            diffuse = Vec3.create 0.0 1.0 0.0;
            specular = Vec3.create 0.0 0.0 0.0;
            shininess = 10.0;
          }
    | `Blue ->
        Phong
          {
            ambient = Vec3.create 0.0 0.0 0.1;
            diffuse = Vec3.create 0.0 0.0 1.0;
            specular = Vec3.create 0.3 0.3 0.3;
            shininess = 10.0;
          }
    | `Mirror -> Reflective

  let get_phong (mat : phong_props) (eye_dir : Vec3.t) (normal : Vec3.t)
      (light_dir : Vec3.t) =
    let ambient = mat.ambient in
    let diffuse = mat.diffuse *@ Vec3.dot normal light_dir |> Vec3.max 0.0 in
    let specular =
      let reflect_dir = Vec3.reflect light_dir normal in
      let spec = Vec3.dot eye_dir reflect_dir in
      if spec < 0.0 then Vec3.zero else mat.specular *@ (spec ** mat.shininess)
    in
    ambient +@ diffuse +@ specular
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

  let get_intersection (prim : t) (ray : Ray.t) =
    let { shape; material } = prim in
    match shape with
    | Sphere { pos; radius } ->
        (* TODO: currently stub code that is not very reliable  *)
        (* also should eventually output an actual intersection type  *)
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

  let get_first_nonlight_intersection (prims : t list) (ray : Ray.t) =
    prims
    |> List.filter_map (fun p -> get_intersection p ray)
    |> List.filter (fun (i : Intersection.t) ->
           match i.material with Light -> false | _ -> true)
    |> min_helper
end
