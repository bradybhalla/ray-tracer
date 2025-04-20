open Math
open Utils

module type ShapeInterface = sig
  type t

  val intersect : t -> Ray.t -> (float * shape_intersection) option
end

type sphere = { pos : Vec3.t; radius : float }

module Sphere : ShapeInterface with type t = sphere = struct
  type t = sphere

  let tex_coord_of_point { pos; _ } point : Texture.tex_coord =
    let offset = Vec3.normalize (point -@ pos) in
    let u = (atan2 offset.x offset.z /. (2.0 *. Float.pi)) +. 0.5 in
    let v = (offset.y *. 0.5) +. 0.5 in
    { u; v }

  let intersect_times { pos; radius } (ray : Ray.t) =
    let a = Vec3.dot ray.dir ray.dir in
    let b = -2.0 *. Vec3.dot ray.dir (pos -@ ray.origin) in
    let c =
      Vec3.dot (pos -@ ray.origin) (pos -@ ray.origin) -. (radius *. radius)
    in
    solve_quadratic a b c

  let intersection_of_times s ray (t1, t2) =
    if t2 < 0.0 then
      (* both intersections are negative *)
      None
    else if t1 > 0.0 then
      (* both are positive (t1 is the min) *)
      Some
        ( t1,
          {
            point = Ray.at ray t1;
            normal = Ray.at ray t1 -@ s.pos |> Vec3.normalize;
            (* TODO: sphere texture coordinate is not there yet *)
            tex_coord = tex_coord_of_point s (Ray.at ray t1);
            medium_transition = Out2In;
          } )
    else
      (* inside sphere (t2 is min positive), flip normal *)
      Some
        ( t2,
          {
            point = Ray.at ray t2;
            normal = s.pos -@ Ray.at ray t2 |> Vec3.normalize;
            (* TODO: sphere texture coordinate is not there yet *)
            tex_coord = tex_coord_of_point s (Ray.at ray t2);
            medium_transition = In2Out;
          } )

  let intersect s ray = intersect_times s ray >>= intersection_of_times s ray
end

type plane = { normal : Vec3.t; pos : Vec3.t; xdir : Vec3.t }

module Plane : ShapeInterface with type t = plane = struct
  type t = plane

  let intersect { normal; pos; xdir } (ray : Ray.t) =
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
          ( t,
            {
              point = Ray.at ray t;
              normal = (if not flip_normal then normal else normal *@ -1.0);
              tex_coord =
                (let u_dir = Vec3.cross xdir normal |> Vec3.normalize in
                 let v_dir = Vec3.cross normal u_dir |> Vec3.normalize in
                 let proj_pos = Ray.at ray t -@ pos in
                 { u = Vec3.dot proj_pos u_dir |> decimal; v = Vec3.dot proj_pos v_dir |> decimal });
              medium_transition = Out2Out;
            } )
end

type t = Sphere of Sphere.t | Plane of Plane.t

let intersect v ray =
  match v with
  | Sphere v -> Sphere.intersect v ray
  | Plane v -> Plane.intersect v ray

(* TODO: make "create" function so t doesn't need to be exposed *)
