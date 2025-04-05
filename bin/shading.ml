open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Object

let const_tracer (prims : Primitive.t list) (ray : Ray.t) =
  match first_ray_intersection ray prims with
  | Some i ->
      let normal = i.normal in
      let light_dir = Vec3.(create 0.0 (-1.0) 0.0 |> normalize) in
      let light_intensity = Vec3.dot normal light_dir |> max 0.0 in
      let c' = i.material.color *@ light_intensity in
      c'
  | None -> Vec3.create 0.0 0.0 0.0

let () =
  Render.create
    ~scene:(Scenes.two_spheres_scene 600)
    ~params:{ samples_per_pixel = 1 } ~tracer:const_tracer
  |> Ppm.of_render |> Ppm.print
