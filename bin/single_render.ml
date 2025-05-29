open Ray_tracer
open Whitted_tracer
open Random_walk_tracer

let scene = Scenes.lens 300 0.2
(* let scene = Scenes.globe 300 0.9 *)
(* let scene = Scenes.room 300 0.9 *)
(* let scene = Scenes.three_spheres 300 0.0 *)
(* let scene = Scenes.onshape 300 0.1 *)

let render () =
  Render.create ~scene
    ~params:{ samples_per_pixel = 100; num_domains = 5 }
    ~tracer:random_walk
  |> Ppm.of_render

let () = render () |> Ppm.print
