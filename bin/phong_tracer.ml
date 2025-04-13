open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Math

let phong (scene : Render.scene) (ray : Ray.t) =
  let is_shadowed (int : Intersection.t) (light : Light.t) =
    match light with
    | Point { pos; _ } -> (
        let light_ray =
          Ray.(create ~origin:pos ~dir:(int.point -@ pos) () |> normalize)
        in
        let int' =
          Primitive.get_first_intersection scene.primitives light_ray
        in
        match int' with
        | None -> false
        | Some i -> not (Vec3.is_close i.point int.point))
  in
  let shade_phong_from_light (props : Material.phong_props)
      (int : Intersection.t) (light : Light.t) =
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
    let int = Primitive.get_first_intersection scene.primitives ray in
    match int with
    | None -> Vec3.zero
    | Some i -> (
        match i.material with
        | Phong props ->
            List.map (shade_phong_from_light props i) scene.lights
            |> List.fold_left ( +@ ) Vec3.zero
        | Reflective ->
            if max_depth <= 0 then Vec3.zero
            else
              let reflected_ray = Ray.reflect ray i in
              Vec3.create 0.02 0.02 0.02 +@ helper reflected_ray (max_depth - 1)
        | Refractive _ ->
            if max_depth <= 0 then Vec3.zero
            else
              let refracted_ray = Ray.refract ray i in
              Vec3.create 0.02 0.02 0.02 +@ helper refracted_ray (max_depth - 1)
        )
  in
  helper ray 10

let render n i = 
  Printf.printf "Processing frame %d/%d" (i+1) n;
  print_newline ();
  Render.create
    ~scene:(Scenes.box_room_scene 800 (float_of_int i /. float_of_int n))
    ~params:{ samples_per_pixel = 10 } ~tracer:phong
  |> Ppm.of_render

let () =
  let n = 30 in
  (* render n 0 |> Ppm.print *)
  let indices = List.init n (Fun.id) in
  List.iter (fun i -> render n i |> Ppm.save (Printf.sprintf "frame_%05d.ppm" i)) indices
