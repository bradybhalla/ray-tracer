open Utils
open Math

type t = {
  shape : Shape.t;
  material : Material.t;
  light : Texture.t option;
  medium : Medium.spec;
}

type intersection = {
  time : float;
  si : shape_intersection;
  (* vector back to ray origin in local si coords
     (x,y,z) -> (si.ds, si.outward_normal x si.ds, si.outward_normal)
   *)
  wo : Vec3.t;
  prim : t;
}

let dir_to_local (v : Vec3.t) (si : shape_intersection) =
  let dt = Vec3.cross si.outward_normal si.ds in
  Vec3.assert_unit v;
  Vec3.assert_unit si.ds;
  Vec3.assert_unit dt;
  Vec3.assert_unit si.outward_normal;
  Vec3.assert_perp si.ds dt;
  Vec3.assert_perp si.ds si.outward_normal;
  Vec3.assert_perp dt si.outward_normal;
  let res =
    Vec3.create (Vec3.dot v si.ds) (Vec3.dot v dt)
      (* TODO: when this is flipped it is normal? *)
      (Vec3.dot v si.outward_normal)
  in
  Vec3.assert_unit res;
  res

let get_intersection ~(ray : Ray.t) ~(primitive : t) =
  let shape_int = Shape.intersect ~shape:primitive.shape ~ray in
  match shape_int with
  | None -> None
  | Some (t, si) ->
      Some
        {
          time = t;
          si;
          prim = primitive;
          (* dir back to ray origin *)
          wo = ray.dir *@ -1.0;
        }
