open Math

let ( >>= ) = Option.bind
let ( =<< ) f x = x >>= f
let lift f = Fun.compose Option.some f

type shape_intersection = {
  point : Vec3.t;
  normal : Vec3.t;
  tex_coord : Texture.tex_coord;
  medium_transition : Medium.transition;
}

type intersection = {
  si : shape_intersection;
  material : Material.t;
  medium_incident : Medium.t;
  medium_transmitted : Medium.t;
}

module Ray = struct
  type t = { origin : Vec3.t; dir : Vec3.t }

  let create ~origin ~dir = { origin; dir }
  let normalize ray = { ray with dir = Vec3.normalize ray.dir }
  let at ray time = ray.origin +@ (ray.dir *@ time)

  let reflect (ray : t) (i : intersection) =
    {
      origin = i.si.point +@ (i.si.normal *@ 0.001);
      dir = Vec3.reflect ray.dir i.si.normal;
    }

  let refract (ray : t) (i : intersection) =
    {
      origin = i.si.point +@ (i.si.normal *@ -0.001);
      dir =
        Vec3.refract ray.dir i.si.normal
          (i.medium_incident /. i.medium_transmitted);
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
