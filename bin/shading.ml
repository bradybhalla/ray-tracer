open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Object

let const_tracer (scene : Render.scene) (ray : Ray.t) =
  let rec helper (ray : Ray.t) (max_depth : int) =
    first_ray_intersection ray scene.primitives
    |> Option.fold ~none:Vec3.zero ~some:(fun (int : Intersection.t) ->
           match int.material with
           | Phong props ->
               List.map
                 (fun light ->
                   match light with
                   | Light.Point { pos; power } ->
                       let light_dir = pos -@ int.point |> Vec3.normalize in
                       let light_dist = Vec3.mag (pos -@ int.point) in
                       Material.get_phong props ray.dir int.normal light_dir
                       *@ (power /. light_dist /. light_dist))
                 scene.lights
               |> List.fold_left ( +@ ) Vec3.zero
           | Reflective ->
               if max_depth <= 0 then Vec3.create 0.05 0.05 0.05
               else
                 let reflect_dir = Vec3.reflect ray.dir int.normal in
                 let reflect_origin = int.point +@ (int.normal *@ 0.001) in
                 Vec3.create 0.05 0.05 0.05
                 +@ helper
                      { origin = reflect_origin; dir = reflect_dir }
                      (max_depth - 1))
  in
  helper ray 5

let () =
  Render.create
    ~scene:(Scenes.two_spheres_scene 600)
    ~params:{ samples_per_pixel = 10 } ~tracer:const_tracer
  |> Ppm.of_render |> Ppm.print
