open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math

let phong (scene : Scene.t) (ray : Ray.t) =
  let get_phong_color (ray : Ray.t) (tex : Texture.t)
      (int : Primitive.intersection) (light : Light.t) =
    let color = Texture.eval tex int.si.tex_coord in
    let ambient = color /@ 10.0 in
    let specular = Vec3.create 0.1 0.1 0.1 in
    let shininess = 10.0 in
    let { brightness_at_point; wi; _ } = Light.sample_Li light int.si in
    let diffuse = color *@ Vec3.dot int.si.normal wi |> Vec3.max 0.0 in
    let specular =
      let reflect_dir = Vec3.reflect wi int.si.normal in
      let spec = Vec3.dot ray.dir reflect_dir in
      if spec < 0.0 then Vec3.zero () else specular *@ (spec ** shininess)
    in
    ambient +@ ((diffuse +@ specular) *@@ brightness_at_point)
  in
  let rec diffuse ray tex i =
    List.map (get_phong_color ray tex i) scene.finite_emitters
    |> List.fold_left ( +@ ) (Vec3.zero ())
  and recurse f ray max_depth i =
    if max_depth <= 0 then Vec3.zero ()
    else
      let recursed_ray = f ray i in
      Vec3.create 0.02 0.02 0.02 +@ helper recursed_ray (max_depth - 1)
  and helper (ray : Ray.t) (max_depth : int) =
    let int = Scene.first_primitive_intersection scene ray in
    match int with
    | None ->
        List.fold_left
          (fun a il -> a +@ Light.get_Le il ray)
          (Vec3.zero ()) scene.infinite_lights
    | Some i -> (
        match i.prim.material with
        | Diffuse { tex; reflect_prob } ->
            if reflect_prob = 0.0 then diffuse ray tex i
            else
              (recurse Ray.reflect ray max_depth i.si *@ reflect_prob)
              +@ (diffuse ray tex i *@ (1.0 -. reflect_prob))
        | Refractive { reflect_prob } ->
            if Sample.float () < reflect_prob then
              recurse Ray.reflect ray max_depth i.si
            else Vec3.zero ())
  in
  helper ray 10
