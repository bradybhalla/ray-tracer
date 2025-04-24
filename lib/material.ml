open Math

type t =
  | Diffuse of { tex : Texture.t; reflect_prob : float }
  | Refractive of { reflect_prob : float }

let tex_red = Texture.Constant (Vec3.create 0.8 0.2 0.2)
let tex_green = Texture.Constant (Vec3.create 0.2 0.8 0.2)
let tex_blue = Texture.Constant (Vec3.create 0.2 0.2 0.8)
let tex_white = Texture.Constant (Vec3.create 0.6 0.6 0.6)
let tex_black = Texture.Constant (Vec3.create 0.1 0.1 0.1)

let tex_checker nu nv =
  Texture.Checkered (nu, nv, Vec3.create 0.1 0.1 0.1, Vec3.create 0.6 0.6 0.6)

let tex_grad nu nv = 
  Texture.Image (nu, nv,
  Array.init nu (fun r ->
    Array.init nv (fun c ->
      let u = float_of_int r /. float_of_int nu in
      let v = float_of_int c /. float_of_int nv in
      Vec3.create u v (1.0 -. u *. v)
  )
  ))

let create = function
  | `Red -> Diffuse { tex = tex_red; reflect_prob = 0.0 }
  | `Green -> Diffuse { tex = tex_green; reflect_prob = 0.0 }
  | `Blue -> Diffuse { tex = tex_blue; reflect_prob = 0.0 }
  | `White -> Diffuse { tex = tex_white; reflect_prob = 0.0 }
  | `Black -> Diffuse { tex = tex_black; reflect_prob = 0.0 }
  | `Checkerboard (nu, nv) ->
      Diffuse { tex = tex_checker nu nv; reflect_prob = 0.0 }
  | `Gradient (nu, nv) ->
      Diffuse { tex = tex_grad nu nv; reflect_prob = 0.0 }
  | `Mirror -> Refractive { reflect_prob = 1.0 }
  | `Glass -> Refractive { reflect_prob = 0.0 }
