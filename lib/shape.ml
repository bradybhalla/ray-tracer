open Math
open Utils

module type ShapeInterface = sig
  type t
  type params

  val create : params -> t
  val intersect : t -> Ray.t -> (float * shape_intersection) option
  val transform : t -> Transform.t -> t
end

type sphere_params = { pos : Vec3.t; radius : float }

module Sphere : ShapeInterface with type params = sphere_params = struct
  (* NOTE: if sphere doesn't store the transforms, it is hard to
         calculate the result of scaling it. Maybe this could be
         done more efficiently with more effort. *)
  type t = {
    pos : Vec3.t;
    radius : float;
    transform : Transform.t;
    inv_transform : Transform.t;
  }

  type params = sphere_params

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
    solve_quadratic a b c

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
            normal = Ray.at ray t1 -@ s.pos |> Vec3.normalize;
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
end

type plane_params = { normal : Vec3.t; pos : Vec3.t }

module Plane : ShapeInterface with type params = plane_params = struct
  type t = { normal : Vec3.t; pos : Vec3.t; udir : Vec3.t; vdir : Vec3.t }
  type params = plane_params

  let transform (plane : t) tr =
    {
      normal = Transform.normal tr plane.normal;
      pos = Transform.point tr plane.pos;
      udir = Transform.vec tr plane.udir;
      vdir = Transform.vec tr plane.vdir;
    }

  let create ({ normal; pos } : params) =
    let default_normal = Vec3.create 0.0 (-1.0) 0.0 in
    let default_u = Vec3.create 1.0 0.0 0.0 in
    let default_v = Vec3.create 0.0 0.0 (-1.0) in
    let cross = Vec3.cross default_normal normal in
    let mag = Vec3.mag cross in
    let default =
      {
        normal = default_normal;
        pos = Vec3.zero;
        udir = default_u;
        vdir = default_v;
      }
    in
    let tr : Transform.t =
      if mag = 0.0 then [ Translation pos ]
      else [ Rotation (Vec3.normalize cross, asin mag); Translation pos ]
    in
    transform default tr

  let intersect { normal; pos; udir; vdir } (ray : Ray.t) =
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
                (let proj_pos = Ray.at ray t -@ pos in
                 {
                   u = Vec3.dot proj_pos udir /. Vec3.mag_sq udir |> decimal;
                   v = Vec3.dot proj_pos vdir /. Vec3.mag_sq vdir |> decimal;
                 });
              medium_transition = Out2Out;
            } )
end

type triangle_params = { p0 : Vec3.t; p1 : Vec3.t; p2 : Vec3.t }

module Triangle : ShapeInterface with type params = triangle_params = struct
  type t = triangle_params
  type params = triangle_params

  let create triangle = triangle

  (* TODO: probably inefficient for now *)
  let get_bary { p0 = a; p1 = b; p2 = c } p =
    let v0 = b -@ a in
    let v1 = c -@ a in
    let v2 = p -@ a in
    let d00 = Vec3.dot v0 v0 in
    let d01 = Vec3.dot v0 v1 in
    let d11 = Vec3.dot v1 v1 in
    let d20 = Vec3.dot v2 v0 in
    let d21 = Vec3.dot v2 v1 in
    let denom = (d00 *. d11) -. (d01 *. d01) in
    let v = ((d11 *. d20) -. (d01 *. d21)) /. denom in
    let w = ((d00 *. d21) -. (d01 *. d20)) /. denom in
    (v, w, 1.0 -. v -. w)

  (* TODO: probably inefficient for now *)
  let intersect triangle (ray : Ray.t) =
    let { p0; p1; p2 } = triangle in
    let normal = Vec3.cross (p1 -@ p0) (p2 -@ p0) |> Vec3.normalize in
    let pos = p0 in
    let num = Vec3.dot (pos -@ ray.origin) normal in
    let denom = Vec3.dot ray.dir normal in
    if denom = 0.0 then
      (* ray parallel to plane of triangle *)
      None
    else
      let t = num /. denom in
      if t < 0.0 then
        (* intersection is negative *)
        None
      else
        let intersect_point = Ray.at ray t in
        let outward_normal =
          if Vec3.dot ray.dir normal > 0.0 then normal *@ -1.0 else normal
        in
        let c0, c1, c2 = get_bary triangle intersect_point in
        if c0 < 0.0 || c0 > 1.0 || c1 < 0.0 || c1 > 1.0 || c2 < 0.0 || c2 > 1.0
        then None
        else
          Some
            ( t,
              {
                point = intersect_point;
                normal = outward_normal;
                tex_coord = { u = 0.0; v = 0.0 };
                medium_transition = Out2Out;
              } )

  let transform triangle tr =
    {
      p0 = Transform.point tr triangle.p0;
      p1 = Transform.point tr triangle.p1;
      p2 = Transform.point tr triangle.p2;
    }
end

type t = Sphere of Sphere.t | Plane of Plane.t | Triangle of Triangle.t

type params =
  | SphereParams of Sphere.params
  | PlaneParams of Plane.params
  | TriangleParams of Triangle.params

let create (p : params) =
  match p with
  | SphereParams p -> Sphere (Sphere.create p)
  | PlaneParams p -> Plane (Plane.create p)
  | TriangleParams p -> Triangle (Triangle.create p)

let intersect (v : t) ray =
  match v with
  | Sphere v -> Sphere.intersect v ray
  | Plane v -> Plane.intersect v ray
  | Triangle v -> Triangle.intersect v ray

let transform (v : t) (tr : Transform.t) : t =
  match v with
  | Sphere v -> Sphere (Sphere.transform v tr)
  | Plane v -> Plane (Plane.transform v tr)
  | Triangle v -> Triangle (Triangle.transform v tr)

(* TODO: make "create" function so t doesn't need to be exposed *)
