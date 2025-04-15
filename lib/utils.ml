open Math

module Intersection = struct
  type t = {
    time : float;
    point : Vec3.t;
    normal : Vec3.t;
    material : Material.t;
    tex_coord: Texture.tex_coord;

    (* the medium on the other side of the intersection *)
    other_medium: Medium.t;
  }
end

module Ray = struct
  type t = { origin : Vec3.t; dir : Vec3.t; medium : Medium.t }

  let create ~origin ~dir ~medium =
    { origin; dir; medium }

  let normalize ray = { ray with dir = Vec3.normalize ray.dir }
  let at ray time = ray.origin +@ (ray.dir *@ time)

  let reflect (ray : t) (i : Intersection.t) =
    {
      origin = i.point +@ (i.normal *@ 0.001);
      dir = Vec3.reflect ray.dir i.normal;
      medium = ray.medium;
    }

  let refract (ray : t) (i : Intersection.t) (med_inc: Medium.t) (med_trans: Medium.t) =
    {
      origin = i.point +@ (i.normal *@ -0.001);
      dir = Vec3.refract ray.dir i.normal (med_inc /. med_trans);
      medium = i.other_medium
    }

end

module Sample = struct
  (* TODO: not correct sampling *)
  let unit_vec3 () =
    let v =
      Vec3.create (Random.float 1.0) (Random.float 1.0) (Random.float 1.0)
    in
    let v = v -@ Vec3.create 0.5 0.5 0.5 in
    Vec3.normalize v
end
