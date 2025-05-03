open Ray_tracer
open Phong_tracer

let render () =
  Render.create
    ~scene:(Scenes.space_scene 300 0.9)
    ~params:{ samples_per_pixel = 5 } ~tracer:phong
  |> Ppm.of_render

let () = render () |> Ppm.print
