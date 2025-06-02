open Math
open Utils

type geometric_light = { shape : Shape.t; brightness : Texture.t }

type infinite_light =
  (* TODO: add octahedral mapping for environment map texture.
           See PBRT 12.5.2 for more info *)
  | Environment of Vec3.t

type t =
  | Geometric of geometric_light
  | Infinite of infinite_light
  | Point of { pos : Vec3.t; brightness : Vec3.t }

(* sample a light to use from the current shape's intersection *)
let sample_Li ~(light : t) ~(prim_si : shape_intersection) =
  let sample_from_point light_point color_at_light =
    let point_to_light = light_point -@ prim_si.point in
    let wi = Vec3.normalize point_to_light in
    {
      brightness_at_light = color_at_light;
      wi;
      light_dist = Some (Vec3.mag point_to_light);
    }
  in
  match light with
  | Infinite (Environment brightness) ->
      let wi = Sample.unit_vec3 () in
      { brightness_at_light = brightness; wi; light_dist = None }
  | Geometric { shape; brightness } ->
      let light_shape_sample = Shape.sample shape in
      sample_from_point light_shape_sample.point
        (Texture.eval brightness light_shape_sample.tex_coord)
  | Point { pos; brightness } -> sample_from_point pos brightness

(* light emitted to ray from infinite sources *)
let escaped_ray_color ~(light : infinite_light) ~(ray : Ray.t) =
  let _ = ray in
  (* remove unused warning *)
  match light with
  | Environment b -> b

(* TODO: see PBRT 12.6 (Light Sampling) for more accurate implementation.
         the light should only be emitted up to a certain angle away from normal.
 *)
let emitted_color ~(light : geometric_light) ~(light_si : shape_intersection) =
  Texture.eval light.brightness light_si.tex_coord
