open Ray_tracer
open Phong_tracer

let render () =
  Render.create
    ~scene:(Scenes.spheres_scene 300 0.2)
    ~params:{ samples_per_pixel = 15 } ~tracer:phong
  |> Ppm.of_render

let () = render () |> Ppm.print
