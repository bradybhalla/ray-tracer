open Ray_tracer
open Phong_tracer

let render () =
  Render.create ~scene:(Scenes.room_scene 200 0.2)
    ~params:{ samples_per_pixel = 10 } ~tracer:phong
  |> Ppm.of_render

let () = render () |> Ppm.print
