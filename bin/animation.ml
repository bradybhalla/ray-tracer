open Ray_tracer
open Phong_tracer

(* let create_scene = Scenes.space_scene 300 *)
(* let create_scene = Scenes.room_scene 300 *)
(* let create_scene = Scenes.spheres_scene 300 *)
let create_scene = Scenes.obj_scene 300

let render n i =
  Printf.printf "Processing frame %d/%d" (i + 1) n;
  print_newline ();
  Render.create
    ~scene:(create_scene (float_of_int i /. float_of_int n))
    ~params:{ samples_per_pixel = 3 } ~tracer:phong
  |> Ppm.of_render

let () =
  let seconds = 1.0 in
  let n = int_of_float (seconds *. 30.0) in
  let indices = List.init n Fun.id in
  List.iter
    (fun i ->
      render n i |> Ppm.save (Printf.sprintf "animation/frame_%05d.ppm" i))
    indices
