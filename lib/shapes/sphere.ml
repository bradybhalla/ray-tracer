open Math
open Utils

(* NOTE: if sphere doesn't store the transforms, it is hard to
         calculate the result of scaling it. Maybe this could be
         done more efficiently with more effort. *)
type t = {
  pos : Vec3.t;
  radius : float;
  transform : Transform.t;
  inv_transform : Transform.t;
}

type params = { pos : Vec3.t; radius : float }

let create ({ pos; radius } : params) =
  {
    pos;
    radius;
    transform = Transform.identity;
    inv_transform = Transform.identity;
  }

let tex_coord_of_point ({ pos; _ } : t) point : Texture.tex_coord =
  let offset = Vec3.normalize (point -@ pos) in
  let u = (offset.y *. 0.5) +. 0.5 in
  let v = (atan2 offset.x offset.z /. (2.0 *. Float.pi)) +. 0.5 in
  { u; v }

let intersect_times ({ pos; radius; _ } : t) (ray : Ray.t) =
  let a = Vec3.dot ray.dir ray.dir in
  let b = -2.0 *. Vec3.dot ray.dir (pos -@ ray.origin) in
  let c =
    Vec3.dot (pos -@ ray.origin) (pos -@ ray.origin) -. (radius *. radius)
  in
  solve_quadratic ~a ~b ~c

let intersection_of_times (s : t) ray (t1, t2) =
  if t2 < 0.0 then
    (* both intersections are negative *)
    None
  else if t1 > 0.0 then
    (* both are positive (t1 is the min) *)
    Some
      ( t1,
        {
          point = Ray.at ray t1;
          outward_normal = Ray.at ray t1 -@ s.pos |> Vec3.normalize;
          tex_coord = tex_coord_of_point s (Ray.at ray t1);
          medium_transition = Out2In;
        } )
  else
    (* inside sphere (t2 is min positive) *)
    Some
      ( t2,
        {
          point = Ray.at ray t2;
          outward_normal = Ray.at ray t2 -@ s.pos |> Vec3.normalize;
          tex_coord = tex_coord_of_point s (Ray.at ray t2);
          medium_transition = In2Out;
        } )

let transform_intersection tr (t, si) =
  Some (t, Transform.shape_intersection tr si)

let intersect (s : t) ray =
  (* ray into sphere coords *)
  let ray = Transform.ray s.inv_transform ray in
  (* find points of intersection *)
  intersect_times s ray
  (* calculate intersection props *)
  >>= intersection_of_times s ray
  (* intersection back to world coords *)
  >>= transform_intersection s.transform

let transform s tr =
  let transform = Transform.compose s.transform tr in
  let inv_transform = Transform.inv transform in
  { s with transform; inv_transform }

let sample s _ =
  let { pos; radius; transform; _ } = s in
  let dir = Sample.unit_vec3 () in
  let point = pos +@ (dir *@ radius) in
  let normal = dir in
  let tex = tex_coord_of_point s point in
  {
    point = point |> Transform.point transform;
    outward_normal = normal |> Transform.normal transform;
    tex_coord = tex;
  }
