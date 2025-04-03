open Ray_tracer
open Ray_tracer.Math

let const_tracer scene ray =
  let intersect = Object.ray_intersection ray scene in
  match intersect with
  | None -> Vec3.create 0. 0. 0.
  | Some _ -> Vec3.create 1. 0.0 0.0

let () =
  let camera =
    Camera.create 30
  in
  let scene = Object.create ~pos:(Vec3.create 1.0 (-1.0) 10.0) ~radius:1.1 in
  let render_params = Render.{ samples_per_pixel = 100 } in
  Render.create ~camera ~scene ~params:render_params ~tracer:const_tracer
  |> Ppm.of_render |> Ppm.print
