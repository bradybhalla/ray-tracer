open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math

let phong (scene : Render.scene) (ray : Ray.t) =
  let is_shadowed (int : intersection) (light : Light.t) =
    match light with
    | Point { pos; _ } ->
        let light_ray =
          Ray.(create ~origin:pos ~dir:(int.si.point -@ pos) |> normalize)
        in
        let int' =
          Primitive.get_first_intersection scene.primitives light_ray
        in
        Option.fold ~none:false
          ~some:(fun i -> not (Vec3.is_close i.si.point int.si.point))
          int'
  in
  let get_phong_color (ray : Ray.t) (tex : Texture.t) (int : intersection)
      (light : Light.t) =
    match light with
    | Point { pos; power } ->
        let light_dir = pos -@ int.si.point |> Vec3.normalize in
        let light_dist = Vec3.mag (pos -@ int.si.point) in
        let color = Texture.eval tex int.si.tex_coord.u int.si.tex_coord.v in
        let shininess = 10.0 in
        let ambient = Vec3.copy color |> Vec3.cdiv 10.0 in
        if is_shadowed int light then
          ambient |> Vec3.cmul (power /. light_dist /. light_dist)
        else
          let diffuse =
            Vec3.copy color
            |> Vec3.cmul (Vec3.dot int.si.normal light_dir)
            |> Vec3.max 0.0
          in
          let specular =
            let reflect_dir = Vec3.reflect' light_dir int.si.normal in
            let spec = Vec3.dot ray.dir reflect_dir in
            if spec < 0.0 then Vec3.zero ()
            else Vec3.zero () |> Vec3.cadd 0.1 |> Vec3.cmul (spec ** shininess)
          in
          ambient |> Vec3.add diffuse |> Vec3.add specular
          |> Vec3.cmul (power /. light_dist /. light_dist)
  in
  let rec diffuse ray tex i =
    List.map (get_phong_color ray tex i) scene.lights
    |> List.fold_left ( +@ ) (Vec3.zero ())
  and recurse f ray max_depth i =
    if max_depth <= 0 then Vec3.zero ()
    else
      let recursed_ray = f ray i in
      helper recursed_ray (max_depth - 1) |> Vec3.cadd 0.02
  and helper (ray : Ray.t) (max_depth : int) =
    let int = Primitive.get_first_intersection scene.primitives ray in
    match int with
    | None -> Vec3.zero ()
    | Some i -> (
        match i.material with
        | Diffuse { tex; reflect_prob } ->
            if reflect_prob = 0.0 then diffuse ray tex i
            else
              Vec3.zero ()
              |> Vec3.mul_add (recurse Ray.reflect ray max_depth i) reflect_prob
              |> Vec3.mul_add (diffuse ray tex i) (1.0 -. reflect_prob)
        | Refractive { reflect_prob } ->
            if Sample.float () < reflect_prob then
              recurse Ray.reflect ray max_depth i
            else recurse Ray.refract ray max_depth i)
  in
  helper ray 10
