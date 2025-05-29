open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math
open Ray_tracer.Primitive

let get_emitted_color int =
  Option.fold ~none:(Vec3.zero ())
    ~some:(fun l -> Light.get_L l int.si)
    int.prim.light

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

let random_walk (scene : Scene.t) (initial_ray : Ray.t) =
  let rec helper (ray : Ray.t) max_depth =
    if max_depth <= 0 then Vec3.create 1.0 0.0 1.0
    else
      match Scene.first_primitive_intersection scene ray with
      | None ->
          sum_over (fun il -> Light.get_Le il ray) scene.infinite_lights
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
                  Ray.create ~origin:(int.si.point +@ (wi *@ 0.001)) ~dir:wi
                in
                let li = helper ray' (max_depth - 1) in
                get_emitted_color int
                +@ (color *@ fcos *@@ li /@ (1.0 /. (4.0 *. Float.pi)))
          | Mirror ->
              helper (Ray.reflect ray int.si) (max_depth - 1)
              +@ get_emitted_color int
          | Glass ->
              let ray' =
                Ray.refract ray int.si int.prim.medium int.si.medium_transition
              in
              helper ray' (max_depth - 1) +@ get_emitted_color int )
  in
  helper initial_ray 5
