open Math
open Utils

type t = { normal : Vec3.t; pos : Vec3.t; udir : Vec3.t; vdir : Vec3.t }
type params = { normal : Vec3.t; pos : Vec3.t }

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
      pos = Vec3.zero ();
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
      (* outward normal always faces the ray since the material is two sided *)
      let flip_normal = Vec3.dot ray.dir normal > 0.0 in
      Some
        ( t,
          {
            point = Ray.at ray t;
            outward_normal = (if not flip_normal then normal else normal *@ -1.0);
            tex_coord =
              (let proj_pos = Ray.at ray t -@ pos in
               {
                 u = Vec3.dot proj_pos udir |> decimal;
                 v = Vec3.dot proj_pos vdir |> decimal;
               });
            medium_transition = Out2Out;
          } )
