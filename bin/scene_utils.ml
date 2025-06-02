open Ray_tracer.Math
open Ray_tracer

(* define textures *)
let tex_red = Texture.Constant (Vec3.create 0.8 0.2 0.2)
let tex_green = Texture.Constant (Vec3.create 0.2 0.8 0.2)
let tex_blue = Texture.Constant (Vec3.create 0.2 0.2 0.8)
let tex_white = Texture.Constant (Vec3.create 0.6 0.6 0.6)
let tex_black = Texture.Constant (Vec3.create 0.1 0.1 0.1)

let tex_checker nu nv =
  Texture.Checkered (nu, nv, Vec3.create 0.1 0.1 0.1, Vec3.create 0.6 0.6 0.6)

let tex_grad nu nv =
  Texture.Image
    ( nu,
      nv,
      Array.init nu (fun r ->
          Array.init nv (fun c ->
              let u = float_of_int r /. float_of_int nu in
              let v = float_of_int c /. float_of_int nv in
              Vec3.create u v (1.0 -. (u *. v)))) )

let tex_earth = Ppm.of_file ~filename:"textures/earth.ppm" |> Ppm.to_texture

(* generate materials from names *)
let mc =
  let open Ray_tracer.Material in
  function
  | `Red -> Diffuse { tex = tex_red }
  | `Green -> Diffuse { tex = tex_green }
  | `Blue -> Diffuse { tex = tex_blue }
  | `White -> Diffuse { tex = tex_white }
  | `Black -> Diffuse { tex = tex_black }
  | `Checkerboard (nu, nv) -> Diffuse { tex = tex_checker nu nv }
  | `Gradient (nu, nv) -> Diffuse { tex = tex_grad nu nv }
  | `Mirror -> Mirror
  | `Glass -> Glass
  | `Earth -> Diffuse { tex = tex_earth }

let ( % ) (prim : Primitive.t) tr =
  { prim with shape = Shape.transform ~tr ~shape:prim.shape }

(* build primitives *)
let sphere_at v r mat : Primitive.t =
  let material = mc mat in
  let res : Primitive.t =
    {
      shape = Shape.create (SphereParams { pos = v; radius = r });
      material;
      light = None;
      medium =
        (match material with
        | Glass -> { inside = 1.5; outside = Medium.default }
        | _ -> Medium.default_spec);
    }
  in
  res

let sphere_on_y1 x z mat r : Primitive.t =
  sphere_at (Vec3.create x (1.0 -. r) z) r mat

let ground mat y : Primitive.t =
  {
    shape =
      Shape.create
        (PlaneParams
           { normal = Vec3.create 0.0 (-1.0) 0.0; pos = Vec3.create 0.0 y 0.0 });
    material = mc mat;
    light = None;
    medium = Medium.default_spec;
  }

let wall_facing_origin (pos : Vec3.t) mat : Primitive.t =
  {
    shape =
      Shape.create (PlaneParams { normal = Vec3.normalize (pos *@ -1.0); pos });
    material = mc mat;
    light = None;
    medium = Medium.default_spec;
  }

let point_light_at pos power =
  Light.Point { pos; brightness = Vec3.create 1.0 1.0 1.0 *@ power }

let triangle_light_at pos r power : Primitive.t =
  let shape =
    Shape.create
      (TriangleParams
         {
           p0 = pos +@ Vec3.create r 0.0 r;
           p1 = pos +@ Vec3.create (-.r) 0.0 (-.r);
           p2 = pos +@ Vec3.create (-.r) 0.0 r;
         })
  in
  {
    shape;
    material = mc `Black;
    medium = Medium.default_spec;
    light =
      Some
        {
          shape;
          brightness = Texture.Constant (Vec3.create 1.0 1.0 1.0 *@ power);
        };
  }
