open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math

let phong (scene : Scene.t) (ray : Ray.t) =
  let is_shadowed (int : primitive_intersection) (light_pos : Vec3.t) =
    let light_ray =
      Ray.(
        create ~origin:light_pos ~dir:(int.si.point -@ light_pos) |> normalize)
    in
    let int' = Scene.first_primitive_intersection scene light_ray in
    Option.fold ~none:false
      ~some:(fun i -> not (Vec3.is_close i.si.point int.si.point))
      int'
  in
  let get_phong_color (ray : Ray.t) (tex : Texture.t)
      (int : primitive_intersection) (light : Light.t) =
    let color = Texture.eval tex int.si.tex_coord.u int.si.tex_coord.v in
    let ambient = color /@ 10.0 in
    let specular = Vec3.create 0.1 0.1 0.1 in
    let shininess = 10.0 in
    match Light.get_sample light int.si.point with
    | None -> Vec3.zero ()
    | Some { dir_to_light; dist_to_light; power; light_pos } ->
        if is_shadowed int light_pos then
          ambient *@ (power /. dist_to_light /. dist_to_light)
        else
          let diffuse =
            color *@ Vec3.dot int.si.normal dir_to_light |> Vec3.max 0.0
          in
          let specular =
            let reflect_dir = Vec3.reflect dir_to_light int.si.normal in
            let spec = Vec3.dot ray.dir reflect_dir in
            if spec < 0.0 then Vec3.zero () else specular *@ (spec ** shininess)
          in
          (ambient +@ diffuse +@ specular)
          *@ (power /. dist_to_light /. dist_to_light)
  in
  let rec diffuse ray tex i =
    List.map (get_phong_color ray tex i) scene.lights
    |> List.fold_left ( +@ ) (Vec3.zero ())
  and recurse f ray max_depth i =
    if max_depth <= 0 then Vec3.zero ()
    else
      let recursed_ray = f ray i in
      Vec3.create 0.02 0.02 0.02 +@ helper recursed_ray (max_depth - 1)
  and helper (ray : Ray.t) (max_depth : int) =
    let int = Scene.first_intersection scene ray in
    match int with
    | NoInt -> Vec3.zero ()
    | LightInt _ -> Vec3.create 1.0 1.0 1.0
    | PrimitiveInt i -> (
        match i.material with
        | Diffuse { tex; reflect_prob } ->
            if reflect_prob = 0.0 then diffuse ray tex i
            else
              (recurse Ray.reflect ray max_depth i *@ reflect_prob)
              +@ (diffuse ray tex i *@ (1.0 -. reflect_prob))
        | Refractive { reflect_prob } ->
            if Sample.float () < reflect_prob then
              recurse Ray.reflect ray max_depth i
            else recurse Ray.refract ray max_depth i)
  in
  helper ray 10
