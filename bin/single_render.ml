open Ray_tracer
open Phong_tracer

let render () =
  Render.create ~scene:(Scenes.spheres_scene 200)
    ~params:{ samples_per_pixel = 10 } ~tracer:phong
  |> Ppm.of_render

let () = render () |> Ppm.print
