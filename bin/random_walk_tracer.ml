open Ray_tracer
open Ray_tracer.Utils
open Ray_tracer.Texture
open Ray_tracer.Math

let bmul ({ x; y; z } : Vec3.t) ({ x = x'; y = y'; z = z' } : Vec3.t) : Vec3.t =
  { x = x *. x'; y = y *. y'; z = z *. z' }

let random_walk (scene : Scene.t) (ray : Ray.t) =
  let rec helper ray max_depth =
    if max_depth <= 0 then Vec3.zero ()
    else
      let first_int = Scene.first_intersection scene ray in
      match first_int with
      | NoInt -> Vec3.zero ()
      | LightInt { power; _ } -> Vec3.create 1.0 1.0 1.0 *@ power
      | PrimitiveInt int -> (
          match int.material with
          | Diffuse { tex; reflect_prob } when reflect_prob < 0.5 ->
              let wi = Sample.unit_vec3 () +@ int.si.normal |> Vec3.normalize in
              let color =
                Texture.eval tex int.si.tex_coord.u int.si.tex_coord.v
              in
              let abs_cos = Vec3.dot ray.dir wi |> Float.abs in
              (* TODO: a better way to have rays offset themselves? *)
              let outgoing_ray =
                Ray.create
                  ~origin:(int.si.point +@ (int.si.normal *@ 0.0001))
                  ~dir:wi
              in
              bmul (helper outgoing_ray (max_depth - 1)) color *@ abs_cos
          | Diffuse {reflect_prob; _}
          | Refractive {reflect_prob} when reflect_prob >= 0.5 ->
              let outgoing_ray = Ray.reflect ray int in
              let wi = outgoing_ray.dir in
              helper outgoing_ray (max_depth - 1)
          | Diffuse _ -> failwith "unreachable"
          | Refractive _ -> Vec3.zero ())
  in
  helper ray 5
