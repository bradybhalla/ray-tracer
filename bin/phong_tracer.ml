open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Entity

let phong (scene : Render.scene) (ray : Ray.t) =
  let is_shadowed (int : Intersection.t) (light : Light.t) =
    match light with
    | Point { pos; _ } ->
        let light_ray : Ray.t =
          { origin = pos; dir = int.point -@ pos |> Vec3.normalize }
        in
        let int' =
          Primitive.get_first_nonlight_intersection scene.primitives light_ray
        in
        Option.map
          (fun (i : Intersection.t) -> not (Vec3.is_close i.point int.point))
          int'
        |> Option.value ~default:true
  in
  let shade_phong_from_light (props : Material.phong_props) (int : Intersection.t)
      (light : Light.t) =
    match light with
    | Point { pos; power } ->
        if is_shadowed int light then props.ambient
        else
          let light_dir = pos -@ int.point |> Vec3.normalize in
          let light_dist = Vec3.mag (pos -@ int.point) in
          Material.get_phong props ray.dir int.normal light_dir
          *@ (power /. light_dist /. light_dist)
  in
  let rec helper (ray : Ray.t) (max_depth : int) =
    Primitive.get_first_intersection scene.primitives ray
    |> Option.fold ~none:Vec3.zero ~some:(fun (int : Intersection.t) ->
           match int.material with
           | Phong props ->
               List.map (shade_phong_from_light props int) scene.lights
               |> List.fold_left ( +@ ) Vec3.zero
           | Reflective ->
               if max_depth <= 0 then Vec3.create 0.05 0.05 0.05
               else
                 let reflect_dir = Vec3.reflect ray.dir int.normal in
                 let reflect_origin = int.point +@ (int.normal *@ 0.001) in
                 Vec3.create 0.1 0.1 0.1
                 +@ helper
                      { origin = reflect_origin; dir = reflect_dir }
                      (max_depth - 1)
           | Light -> Vec3.create 1.0 1.0 1.0)
  in
  helper ray 10

let () =
  Render.create
    ~scene:(Scenes.two_spheres_scene 800)
    ~params:{ samples_per_pixel = 10 } ~tracer:phong
  |> Ppm.of_render |> Ppm.print
