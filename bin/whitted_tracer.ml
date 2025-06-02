open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Primitive
open Ray_tracer.Math

let get_diffuse_color scene int light tex =
  let color = Texture.eval tex int.si.tex_coord in
  let sample = Light.sample_Li ~light ~prim_si:int.si in
  if Scene.is_shadowed ~scene ~int ~sample then Vec3.zero ()
  else
    let brightness =
      match sample.light_dist with
      | Some d ->
          sample.brightness_at_light /@ Float.pow d 2.0 (* attenuated light *)
      | None -> sample.brightness_at_light (* infinite light (no attenuation) *)
    in
    brightness
    *@ (Vec3.dot sample.wi int.si.outward_normal |> max 0.0)
    *@@ color

let get_emitted_color light_int =
  Option.fold ~none:(Vec3.zero ())
    ~some:(fun light -> Light.emitted_color ~light ~light_si:light_int.si)
    light_int.prim.light

let rec whitted ~(scene : Scene.t) ~(ray : Ray.t) ~max_depth =
  if max_depth = 0 then Vec3.zero ()
  else
    match Scene.first_primitive_intersection ~scene ~ray with
    | None ->
        sum_over
          (fun il -> Light.escaped_ray_color ~light:il ~ray)
          scene.infinite_lights
    | Some int -> (
        match int.prim.material with
        | Diffuse { tex } ->
            sum_over
              (fun l -> get_diffuse_color scene int l tex)
              scene.all_lights
            +@ get_emitted_color int
        | Mirror ->
            whitted ~scene ~ray:(Ray.reflect ray int.si)
              ~max_depth:(max_depth - 1)
            +@ get_emitted_color int
        | Glass ->
            let ray' =
              Ray.refract ray int.si int.prim.medium int.si.medium_transition
            in
            whitted ~scene ~ray:ray' ~max_depth:(max_depth - 1)
            +@ get_emitted_color int
        | BSDF _ ->
            let tex = Texture.Constant (Vec3.create 1.0 0.0 1.0) in
            sum_over
              (fun l -> get_diffuse_color scene int l tex)
              scene.all_lights
            +@ get_emitted_color int)
