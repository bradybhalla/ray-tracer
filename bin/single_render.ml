open Ray_tracer
open Phong_tracer
open Random_walk_tracer

(* let scene = Scenes.space_scene 300 0.9 *)
(* let scene = Scenes.room_scene 300 0.9 *)
(* let scene = Scenes.spheres_scene 300 0.9 *)
(* let scene = Scenes.obj_scene 300 0.1 *)
let scene = Scenes.test_scene 800 0.0

let render () =
  Render.create ~scene
    ~params:{ samples_per_pixel = 300; num_domains = 5 }
    ~tracer:random_walk
  |> Ppm.of_render

let () = render () |> Ppm.print
