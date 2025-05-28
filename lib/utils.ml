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

(* light sample from a point *)
type light_sample = {
  (* attenuated brightness *)
  brightness_at_point : Vec3.t;
  (* direction to light *)
  wi : Vec3.t;
  (* absolute value of cos of angle between normal and wi *)
  (* brightness reaching point should be `brightness_at_point *. cosi` *)
  cosi : float;
}

module Ray = struct
  type t = { origin : Vec3.t; dir : Vec3.t }

  let create ~origin ~dir = { origin; dir }
  let normalize ray = { ray with dir = Vec3.normalize ray.dir }
  let at ray time = ray.origin +@ (ray.dir *@ time)

  let reflect (ray : t) (si : shape_intersection) =
    {
      origin = si.point +@ (si.normal *@ 0.001);
      dir = Vec3.reflect ray.dir si.normal;
    }

  let refract (ray : t) (si : shape_intersection) mspec mtrans =
    {
      origin = si.point +@ (si.normal *@ -0.001);
      dir =
        Vec3.refract ray.dir si.normal
          (Medium.get_incident mspec mtrans
          /. Medium.get_transmitted mspec mtrans);
    }
end
