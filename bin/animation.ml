open Ray_tracer
open Phong_tracer

let render n i =
  Printf.printf "Processing frame %d/%d" (i + 1) n;
  print_newline ();
  Render.create
    ~scene:(Scenes.space_scene 600 (float_of_int i /. float_of_int n))
    ~params:{ samples_per_pixel = 15 } ~tracer:phong
  |> Ppm.of_render

let () =
  let n = 45 in
  let indices = List.init n Fun.id in
  List.iter
    (fun i ->
      render n i |> Ppm.save (Printf.sprintf "animation/frame_%05d.ppm" i))
    indices
