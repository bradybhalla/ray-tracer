open Math
open Utils

type t = {
  p0 : Vec3.t;
  p1 : Vec3.t;
  p2 : Vec3.t;
  tex0 : Texture.tex_coord;
  tex1 : Texture.tex_coord;
  tex2 : Texture.tex_coord;
  normal : Vec3.t;
  v0 : Vec3.t;
  v1 : Vec3.t;
  d00 : float;
  d01 : float;
  d11 : float;
}

type params = { p0 : Vec3.t; p1 : Vec3.t; p2 : Vec3.t }

let create ({ p0; p1; p2 } : params) : t =
  let v0 = p1 -@ p0 in
  let v1 = p2 -@ p0 in
  {
    p0;
    p1;
    p2;
    (* TODO: should use interpolated normals and have tex coords *)
    normal = Vec3.cross v0 v1 |> Vec3.normalize;
    tex0 = { u = 0.0; v = 0.0 };
    tex1 = { u = 0.0; v = 0.0 };
    tex2 = { u = 0.0; v = 0.0 };
    (* precomputed values for later *)
    v0;
    v1;
    d00 = Vec3.dot v0 v0;
    d01 = Vec3.dot v0 v1;
    d11 = Vec3.dot v1 v1;
  }

let get_bary { p0 = a; v0; v1; d00; d11; d01; _ } p =
  let v2 = p -@ a in
  let d20 = Vec3.dot v2 v0 in
  let d21 = Vec3.dot v2 v1 in
  let denom = (d00 *. d11) -. (d01 *. d01) in
  let v = ((d11 *. d20) -. (d01 *. d21)) /. denom in
  let w = ((d00 *. d21) -. (d01 *. d20)) /. denom in
  (v, w, 1.0 -. v -. w)

let point_from_bary ({ p0; p1; p2; _ } : t) a b c =
  (p0 *@ a) +@ (p1 *@ b) +@ (p2 *@ c)

let tex_from_bary ({ tex0; tex1; tex2; _ } : t) a b c : Texture.tex_coord =
  {
    u = (tex0.u *. a) +. (tex1.u *. b) +. (tex2.u *. c);
    v = (tex0.v *. a) +. (tex1.v *. b) +. (tex2.v *. c);
  }

let intersect (triangle : t) (ray : Ray.t) =
  let ({ p0; normal; _ } : t) = triangle in
  let num = Vec3.dot (p0 -@ ray.origin) normal in
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
              outward_normal;
              tex_coord = { u = 0.0; v = 0.0 };
              medium_transition = Out2Out;
            } )

let transform (triangle : t) tr =
  create
    {
      p0 = Transform.point tr triangle.p0;
      p1 = Transform.point tr triangle.p1;
      p2 = Transform.point tr triangle.p2;
    }

let sample (triangle : t) (from : Vec3.t) : shape_sample =
  let a = Sample.float 1.0 in
  let b = Sample.float 1.0 in
  let c = 1.0 -. a -. b in
  let point = point_from_bary triangle a b c in
  {
    point;
    (* two sided shape should have outward normal facing point *)
    (* this means the vector from `from` to `point` should point opposite to the normal *)
    outward_normal =
      (if Vec3.dot (point -@ from) triangle.normal > 0.0 then
         triangle.normal *@ -1.0
       else triangle.normal);
    tex_coord = tex_from_bary triangle a b c;
  }
