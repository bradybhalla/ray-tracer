open Ray_tracer
open Phong_tracer

let render n i =
  Printf.printf "Processing frame %d/%d" (i + 1) n;
  print_newline ();
  Render.create
    ~scene:(Scenes.space_scene 800 (float_of_int i /. float_of_int n))
    ~params:{ samples_per_pixel = 20 } ~tracer:phong
  |> Ppm.of_render

let () =
  let seconds = 3 in
  let n = seconds * 30 in
  let indices = List.init n Fun.id in
  List.iter
    (fun i ->
      render n i |> Ppm.save (Printf.sprintf "animation/frame_%05d.ppm" i))
    indices
