open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Math
open Ray_tracer.Primitive

let get_emitted_color light_int =
  Option.fold ~none:(Vec3.zero ())
    ~some:(fun light -> Light.emitted_color ~light ~light_si:light_int.si)
    light_int.prim.light

(* TODO: make dependend on color instead of multiplying in diffuse *)
(* TODO: also needs to transform to local coords for almost all bsdfs *)
let bsdf wo wi normal =
  let same_hemisphere =
    let wo_sgn = Vec3.dot wo normal > 0.0 in
    let wi_sgn = Vec3.dot wi normal > 0.0 in
    wo_sgn = wi_sgn
  in
  let reflectance = 0.2 in
  if same_hemisphere then reflectance *. 1.0 /. Float.pi else 0.0

let rec random_walk ~(scene : Scene.t) ~(ray : Ray.t) ~max_depth =
  if max_depth <= 0 then Vec3.create 0.0 0.0 0.0
  else
    match Scene.first_primitive_intersection ~scene ~ray with
    | None ->
        sum_over
          (fun il -> Light.escaped_ray_color ~light:il ~ray)
          scene.infinite_lights
    | Some int -> (
        match int.prim.material with
        | Diffuse { tex } ->
            let color = Texture.eval tex int.si.tex_coord in
            let wi = Sample.unit_vec3 () in
            let fcos =
              bsdf int.wo wi int.si.normal
              *. (Vec3.dot wi int.si.normal |> abs_float)
            in
            if fcos = 0.0 then get_emitted_color int
            else
              let ray' =
                Ray.create ~origin:(int.si.point +@ (wi *@ eps)) ~dir:wi
              in
              let li =
                random_walk ~scene ~ray:ray' ~max_depth:(max_depth - 1)
              in
              get_emitted_color int
              +@ (color *@ fcos *@@ li /@ (1.0 /. (4.0 *. Float.pi)))
        | Mirror ->
            random_walk ~scene ~ray:(Ray.reflect ray int.si)
              ~max_depth:(max_depth - 1)
            +@ get_emitted_color int
        | Glass ->
            let ray' =
              Ray.refract ray int.si int.prim.medium int.si.medium_transition
            in
            random_walk ~scene ~ray:ray' ~max_depth:(max_depth - 1)
            +@ get_emitted_color int
        | BSDF bsdf -> failwith "TODO")
