open Ray_tracer
open Phong_tracer

(* let scene = Scenes.space_scene 300 0.9 *)
(* let scene = Scenes.room_scene 300 0.9 *)
(* let scene = Scenes.spheres_scene 300 0.9 *)
let scene = Scenes.obj_scene 300 0.1

let render () =
  Render.create ~scene ~params:{ samples_per_pixel = 3 } ~tracer:phong
  |> Ppm.of_render

let () = render () |> Ppm.print
