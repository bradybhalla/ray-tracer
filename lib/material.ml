open Math
open Texture

type phong_props = {
  ambient : Vec3.t;
  diffuse : Vec3.t;
  specular : Vec3.t;
  shininess : float;
}

type t = Diffuse of ColorTexture.t | Reflective | Refractive

let tex_red = ColorTexture.Constant (Vec3.create 0.8 0.2 0.2)
let tex_green = ColorTexture.Constant (Vec3.create 0.2 0.8 0.2)
let tex_blue = ColorTexture.Constant (Vec3.create 0.2 0.2 0.8)
let tex_white = ColorTexture.Constant (Vec3.create 0.6 0.6 0.6)
let tex_black = ColorTexture.Constant (Vec3.create 0.1 0.1 0.1)
let tex_chess = ColorTexture.Checkered (Vec3.create 0.1 0.1 0.1, Vec3.create 0.6 0.6 0.6)

let create = function
  | `Red -> Diffuse tex_red
  | `Green -> Diffuse tex_green
  | `Blue -> Diffuse tex_blue
  | `White -> Diffuse tex_white
  | `Black -> Diffuse tex_black
  | `Chessboard -> Diffuse tex_chess
  | `Mirror -> Reflective
  | `Glass -> Refractive

let get_phong (mat : phong_props) (eye_dir : Vec3.t) (normal : Vec3.t)
    (light_dir : Vec3.t) =
  let ambient = mat.ambient in
  let diffuse = mat.diffuse *@ Vec3.dot normal light_dir |> Vec3.max 0.0 in
  let specular =
    let reflect_dir = Vec3.reflect light_dir normal in
    let spec = Vec3.dot eye_dir reflect_dir in
    if spec < 0.0 then Vec3.zero else mat.specular *@ (spec ** mat.shininess)
  in
  ambient +@ diffuse +@ specular
