open Math

type phong_props = {
  ambient : Vec3.t;
  diffuse : Vec3.t;
  specular : Vec3.t;
  shininess : float;
}

type t = Diffuse of Texture.t | Reflective | Refractive

let tex_red = Texture.Constant (Vec3.create 0.8 0.2 0.2)
let tex_green = Texture.Constant (Vec3.create 0.2 0.8 0.2)
let tex_blue = Texture.Constant (Vec3.create 0.2 0.2 0.8)
let tex_white = Texture.Constant (Vec3.create 0.6 0.6 0.6)
let tex_black = Texture.Constant (Vec3.create 0.1 0.1 0.1)

let tex_checker nu nv =
  Texture.Checkered (nu, nv, Vec3.create 0.1 0.1 0.1, Vec3.create 0.6 0.6 0.6)

let create = function
  | `Red -> Diffuse tex_red
  | `Green -> Diffuse tex_green
  | `Blue -> Diffuse tex_blue
  | `White -> Diffuse tex_white
  | `Black -> Diffuse tex_black
  | `Checkerboard (nu, nv) -> Diffuse (tex_checker nu nv)
  | `Mirror -> Reflective
  | `Glass -> Refractive
