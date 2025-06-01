open Ray_tracer
open Whitted_tracer
open Random_walk_tracer

(* let create_scene = Scenes.globe 300 *)
let create_scene = Scenes.lens 50
(* let create_scene = Scenes.room 300 *)
(* let create_scene = Scenes.three_spheres 300 *)
(* let create_scene = Scenes.onshape 300 *)

let render n i =
  Printf.printf "Processing frame %d/%d" (i + 1) n;
  print_newline ();
  Render.create
    ~scene:(create_scene (float_of_int i /. float_of_int n))
    ~params:{ samples_per_pixel = 300; num_domains = 5 }
    ~tracer:random_walk
  |> Ppm.of_render

let () =
  let seconds = 1.0 in
  let n = int_of_float (seconds *. 30.0) in
  let indices = List.init n Fun.id in
  List.iter
    (fun i ->
      Ppm.save
        ~filename:(Printf.sprintf "animation/frame_%05d.ppm" i)
        ~ppm:(render n i))
    indices
