open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math

let phong (scene : Render.scene) (ray : Ray.t) =
  let is_shadowed (int : Intersection.t) (light : Light.t) =
    match light with
    | Point { pos; _ } -> (
        let light_ray =
          Ray.(
            create ~origin:pos ~dir:(int.point -@ pos) ~medium:Medium.default
            |> normalize)
        in
        let int' =
          Primitive.get_first_intersection scene.primitives light_ray
        in
        match int' with
        | None -> false
        | Some i -> not (Vec3.is_close i.point int.point))
  in
  let get_phong_color (tex : ColorTexture.t) (int : Intersection.t)
      (light : Light.t) =
    match light with
    | Point { pos; power } ->
        let color = ColorTexture.eval tex int.tex_coord.u int.tex_coord.v in
        let ambient = color /@ 10.0 in
        let specular = Vec3.create 0.1 0.1 0.1 in
        let shininess = 10.0 in
        if is_shadowed int light then ambient
        else
          let light_dir = pos -@ int.point |> Vec3.normalize in
          let light_dist = Vec3.mag (pos -@ int.point) in
          let diffuse =
            color *@ Vec3.dot int.normal light_dir |> Vec3.max 0.0
          in
          let specular =
            let reflect_dir = Vec3.reflect light_dir int.normal in
            let spec = Vec3.dot ray.dir reflect_dir in
            if spec < 0.0 then Vec3.zero else specular *@ (spec ** shininess)
          in
          (ambient +@ diffuse +@ specular) *@ (power /. light_dist /. light_dist)
  in
  let rec helper (ray : Ray.t) (max_depth : int) =
    let int = Primitive.get_first_intersection scene.primitives ray in
    match int with
    | None -> Vec3.zero
    | Some i -> (
        match i.material with
        | Diffuse color_tex ->
            List.map (get_phong_color color_tex i) scene.lights
            |> List.fold_left ( +@ ) Vec3.zero
        | Reflective ->
            if max_depth <= 0 then Vec3.zero
            else
              let reflected_ray = Ray.reflect ray i in
              Vec3.create 0.02 0.02 0.02 +@ helper reflected_ray (max_depth - 1)
        | Refractive ->
            if max_depth <= 0 then Vec3.zero
            else
              let refracted_ray = Ray.refract ray i ray.medium i.other_medium in
              Vec3.create 0.02 0.02 0.02 +@ helper refracted_ray (max_depth - 1)
        )
  in
  helper ray 10
