open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math

(* TODO: col/row/camera not needed if level of detail done through tangent plane *)
let phong (scene : Render.scene) (ray : Ray.t) (`Col x) (`Row y) (camera : Camera.t) =
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
          ~some:(fun (i,_) -> not (Vec3.is_close i.si.point int.si.point))
          int'
  in
  let get_phong_color (ray : Ray.t) (tex : Texture.t) (lod : float) (int : intersection)
      (light : Light.t) =
    match light with
    | Point { pos; power } ->
        let light_dir = pos -@ int.si.point |> Vec3.normalize in
        let light_dist = Vec3.mag (pos -@ int.si.point) in
        let color = Texture.eval tex int.si.tex_coord.u int.si.tex_coord.v lod in
        let ambient = color /@ 10.0 in
        let specular = Vec3.create 0.1 0.1 0.1 in
        let shininess = 10.0 in
        if is_shadowed int light then
          ambient *@ (power /. light_dist /. light_dist)
        else
          let diffuse =
            color *@ Vec3.dot int.si.normal light_dir |> Vec3.max 0.0
          in
          let specular =
            let reflect_dir = Vec3.reflect light_dir int.si.normal in
            let spec = Vec3.dot ray.dir reflect_dir in
            if spec < 0.0 then Vec3.zero else specular *@ (spec ** shininess)
          in
          (ambient +@ diffuse +@ specular) *@ (power /. light_dist /. light_dist)
  in
  let rec diffuse ray tex lod i =
    List.map (get_phong_color ray tex lod i) scene.lights
    |> List.fold_left ( +@ ) Vec3.zero
  and recurse f ray max_depth i =
    if max_depth <= 0 then Vec3.zero
    else
      let recursed_ray = f ray i in
      Vec3.create 0.02 0.02 0.02 +@ helper recursed_ray (max_depth - 1)
  and helper (ray : Ray.t) (max_depth : int) =
    let int = Primitive.get_first_intersection scene.primitives ray in
    match int with
    | None -> Vec3.zero
    | Some (i, shape) -> (
        let lod = Camera.get_level_of_detail camera (`Col x) (`Row y) shape i.si in
        match i.material with
        | Diffuse { tex; reflect_prob } ->
            if reflect_prob = 0.0 then diffuse ray tex lod i
            else
              (recurse Ray.reflect ray max_depth i *@ reflect_prob)
              +@ (diffuse ray tex lod i *@ (1.0 -. reflect_prob))
        | Refractive { reflect_prob } ->
            if Sample.float () < reflect_prob then
              recurse Ray.reflect ray max_depth i
            else recurse Ray.refract ray max_depth i)
  in
  helper ray 10
