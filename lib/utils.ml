open Math

let ( >>= ) = Option.bind
let ( =<< ) f x = x >>= f
let lift f = Fun.compose Option.some f

(* right now assumes all shapes are two sided *)
type shape_intersection = {
  point : Vec3.t;
  tex_coord : Texture.tex_coord;
  medium_transition : Medium.transition;
  (* local basis *)
  ds : Vec3.t;
  outward_normal : Vec3.t;
  (* dt calcualted by outward_normal x ds *)
}

(* light sample from a point *)
type light_sample = {
  (* brightness *)
  brightness_at_light : Vec3.t;
  (* direction to light *)
  wi : Vec3.t;
  (* dist for a ray to reach light location
     used for shadow rays *)
  light_dist : float option;
}

(* shape sample *)
type shape_sample = {
  point : Vec3.t;
  outward_normal : Vec3.t;
  tex_coord : Texture.tex_coord;
}

module Ray = struct
  type t = { origin : Vec3.t; dir : Vec3.t }

  let create ~origin ~dir = { origin; dir }
  let normalize ray = { ray with dir = Vec3.normalize ray.dir }
  let at ray time = ray.origin +@ (ray.dir *@ time)

  let reflect (ray : t) (si : shape_intersection) =
    let normal =
      Medium.ray_side_normal ~transition:si.medium_transition
        ~outward_normal:si.outward_normal
    in
    { origin = si.point +@ (normal *@ eps); dir = Vec3.reflect ray.dir normal }

  let refract (ray : t) (si : shape_intersection) mspec mtrans =
    let normal =
      Medium.ray_side_normal ~transition:si.medium_transition
        ~outward_normal:si.outward_normal
    in
    {
      origin = si.point +@ (normal *@ -.eps);
      dir =
        Vec3.refract ray.dir normal
          (Medium.get_incident ~spec:mspec ~transition:mtrans
          /. Medium.get_transmitted ~spec:mspec ~transition:mtrans);
    }
end

let sum_over f l = List.fold_left (fun a v -> a +@ f v) (Vec3.zero ()) l
