open Math
open Utils

type bsdf = wo:Vec3.t -> wi:Vec3.t -> Vec3.t
type t = Diffuse of { tex : Texture.t } | Glass | Mirror | BSDF of bsdf

let to_bsdf ~(mat : t) ~(si : shape_intersection) : bsdf =
  match mat with
  | Diffuse { tex : Texture.t } ->
      let color = Texture.eval tex si.tex_coord in
      fun ~wo ~wi ->
        let same_hemisphere = wo.z *. wi.z > 0.0 in
        let reflectance = 0.25 in
        let frac =
          if same_hemisphere then reflectance *. 1.0 /. Float.pi else 0.0
        in
        color *@ frac
  | Glass | Mirror -> failwith "TODO"
  | BSDF f -> f
