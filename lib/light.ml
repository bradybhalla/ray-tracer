open Math
open Utils

(* TODO: ideally would just use the primitive's shape *)
type geometric_light = { shape : Shape.t; brightness : Texture.t }

type infinite_light =
  | Environment of
      Vec3.t (* TODO: add octahedral mapping for environment map texture *)

type t =
  | Geometric of geometric_light
  | Infinite of infinite_light
  | Point of { pos : Vec3.t; brightness : Vec3.t }

(* TODO: make this an option *)
let infinite_time = 999999.0

let sample_Li (light : t) (si : shape_intersection) =
  let sample_from_point light_point color_at_light =
    let point_to_light = light_point -@ si.point in
    let wi = Vec3.normalize point_to_light in
    {
      (* TODO: should not attenuate in path tracers *)
      brightness_at_point = color_at_light /@ Vec3.mag_sq point_to_light;
      wi;
      light_dist = Vec3.mag point_to_light;
    }
  in
  match light with
  | Infinite (Environment brightness) ->
      let wi = Sample.unit_vec3 () in
      { brightness_at_point = brightness; wi; light_dist = infinite_time }
  | Geometric { shape; brightness } ->
      let light_pos = Shape.sample_point shape in
      sample_from_point light_pos (Texture.eval brightness si.tex_coord)
  | Point { pos; brightness } -> sample_from_point pos brightness

let get_Le (light : infinite_light) (ray : Ray.t) =
  match light with
  | Environment b -> b

(* TODO: should depend on ray direction *)
let get_L (light : geometric_light) (si : shape_intersection) =
  Texture.eval light.brightness si.tex_coord
