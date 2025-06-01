open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Primitive
open Ray_tracer.Math

let get_diffuse_color scene int light tex =
  let color = Texture.eval tex int.si.tex_coord in
  let sample = Light.sample_Li ~light ~si:int.si in
  if Scene.is_shadowed ~scene ~int ~sample then Vec3.zero ()
  else
    sample.brightness_at_point
    *@ (Vec3.dot sample.wi int.si.normal |> max 0.0)
    *@@ color

let get_emitted_color int =
  Option.fold ~none:(Vec3.zero ())
    ~some:(fun light -> Light.get_L ~light ~si:int.si)
    int.prim.light

let whitted ~(scene : Scene.t) ~(ray : Ray.t) =
  let initial_ray = ray in
  let rec helper (ray : Ray.t) (max_depth : int) =
    if max_depth = 0 then Vec3.zero ()
    else
      match Scene.first_primitive_intersection ~scene ~ray with
      | None ->
          sum_over (fun il -> Light.get_Le ~light:il ~ray) scene.infinite_lights
      | Some int -> (
          match int.prim.material with
          | Diffuse { tex } ->
              sum_over
                (fun l -> get_diffuse_color scene int l tex)
                scene.all_lights
              +@ get_emitted_color int
          | Mirror ->
              helper (Ray.reflect ray int.si) (max_depth - 1)
              +@ get_emitted_color int
          | Glass ->
              let ray' =
                Ray.refract ray int.si int.prim.medium int.si.medium_transition
              in
              helper ray' (max_depth - 1) +@ get_emitted_color int)
  in
  helper initial_ray 5
