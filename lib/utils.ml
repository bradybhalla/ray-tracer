open Math

module Intersection = struct
  type t = {
    time : float;
    point : Vec3.t;
    normal : Vec3.t;
    material : Material.t;
    (* refractive index on the other side of the intersection *)
    flipped_refractive_index : float;
  }
end

module Ray = struct
  type t = { origin : Vec3.t; dir : Vec3.t; refractive_index : float }

  let create ~origin ~dir ?(refractive_index = 1.0) () =
    { origin; dir; refractive_index }

  let normalize ray = { ray with dir = Vec3.normalize ray.dir }
  let at ray time = ray.origin +@ (ray.dir *@ time)

  let reflect (ray : t) (i : Intersection.t) =
    {
      origin = i.point +@ (i.normal *@ 0.001);
      dir = Vec3.reflect ray.dir i.normal;
      refractive_index = ray.refractive_index;
    }

  let refract (ray : t) (i : Intersection.t) =
    let ni_nt = ray.refractive_index /. i.flipped_refractive_index in
    let cos_t = -.Vec3.dot ray.dir i.normal in
    let perp = (ray.dir +@ (i.normal *@ cos_t)) *@ ni_nt in
    let parallel = i.normal *@ -.sqrt (1.0 -. Vec3.mag_sq perp) in
    {
      origin = i.point -@ (i.normal *@ 0.001);
      dir = perp +@ parallel;
      refractive_index = i.flipped_refractive_index;
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
