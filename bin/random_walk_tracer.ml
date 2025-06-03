open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Math
open Ray_tracer.Primitive

let get_emitted_color light_int =
  Option.fold ~none:(Vec3.zero ())
    ~some:(fun light -> Light.emitted_color ~light ~light_si:light_int.si)
    light_int.prim.light

let rec random_walk ~(scene : Scene.t) ~(ray : Ray.t) ~max_depth =
  if max_depth <= 0 then Vec3.create 0.0 0.0 0.0
  else
    match Scene.first_primitive_intersection ~scene ~ray with
    | None ->
        sum_over
          (fun il -> Light.escaped_ray_color ~light:il ~ray)
          scene.infinite_lights
    | Some int ->
        let emitted = get_emitted_color int in
        let bsdf = Material.to_bsdf ~mat:int.prim.material ~si:int.si in
        let wi = Sample.unit_vec3 () in
        let fcos =
          bsdf
            ~wo:(Primitive.dir_to_local int.wo int.si)
            ~wi:(Primitive.dir_to_local wi int.si)
          *@ (Vec3.dot wi int.si.outward_normal |> abs_float)
        in
        let li =
          if Vec3.mag_sq fcos = 0.0 then Vec3.zero ()
          else
            let ray' =
              Ray.create ~origin:(int.si.point +@ (wi *@ eps)) ~dir:wi
            in
            random_walk ~scene ~ray:ray' ~max_depth:(max_depth - 1)
        in
        emitted +@ (fcos *@@ li /@ (1.0 /. (4.0 *. Float.pi)))
