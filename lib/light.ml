open Math
open Utils

type geometric_light = { shape : Shape.t; brightness : Texture.t }
type infinite_light = Uniform of Vec3.t

type t =
  | Geometric of geometric_light
  | Infinite of infinite_light
  | Point of { pos : Vec3.t; brightness : Vec3.t }

let sample_Li (light : t) (si : shape_intersection) =
  let sample_from_point light_point color_at_light =
    let point_to_light = light_point -@ si.point in
    let wi = Vec3.normalize point_to_light in
    {
      brightness_at_point = color_at_light /@ Vec3.mag_sq point_to_light;
      wi;
      cosi = Vec3.dot wi si.normal |> abs_float;
    }
  in
  match light with
  | Infinite (Uniform brightness) ->
      let wi = Sample.unit_vec3 () in
      {
        brightness_at_point = brightness;
        wi;
        cosi = Vec3.dot wi si.normal |> abs_float;
      }
  | Geometric { shape; brightness } ->
      let light_pos = Shape.sample_point shape in
      sample_from_point light_pos (Texture.eval brightness si.tex_coord)
  | Point { pos; brightness } -> sample_from_point pos brightness

let get_Le (light : infinite_light) (ray : Ray.t) =
  match light with Uniform b -> b

let get_L (light : geometric_light) (si : shape_intersection) =
  Texture.eval light.brightness si.tex_coord
