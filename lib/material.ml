open Math

type phong_props = {
  ambient : Vec3.t;
  diffuse : Vec3.t;
  specular : Vec3.t;
  shininess : float;
}

type t = Phong of phong_props | Reflective | Refractive of float

let create = function
  | `Red ->
      Phong
        {
          ambient = Vec3.create 0.1 0.0 0.0;
          diffuse = Vec3.create 0.8 0.2 0.2;
          specular = Vec3.create 0.1 0.1 0.1;
          shininess = 10.0;
        }
  | `Green ->
      Phong
        {
          ambient = Vec3.create 0.0 0.1 0.0;
          diffuse = Vec3.create 0.2 0.8 0.2;
          specular = Vec3.create 0.1 0.1 0.1;
          shininess = 10.0;
        }
  | `Blue ->
      Phong
        {
          ambient = Vec3.create 0.0 0.0 0.1;
          diffuse = Vec3.create 0.2 0.2 0.8;
          specular = Vec3.create 0.1 0.1 0.1;
          shininess = 10.0;
        }
  | `White ->
      Phong
        {
          ambient = Vec3.create 0.1 0.1 0.1;
          diffuse = Vec3.create 0.6 0.6 0.6;
          specular = Vec3.create 0.0 0.0 0.0;
          shininess = 10.0;
        }
  | `Mirror -> Reflective
  | `Glass index -> Refractive index

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
