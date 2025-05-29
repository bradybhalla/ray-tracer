open Math
open Utils

type t = { p0 : Vec3.t; p1 : Vec3.t; p2 : Vec3.t }
type params = t

let create (triangle : params) : t = triangle

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

let point_from_bary { p0; p1; p2 } a b c = (p0 *@ a) +@ (p1 *@ b) +@ (p2 *@ c)

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

let sample_point triangle =
  let a = Sample.float () in
  let b = Sample.float () in
  let c = 1.0 -. a -. b in
  point_from_bary triangle a b c
