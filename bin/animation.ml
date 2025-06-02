open Ray_tracer
module T = Domainslib.Task

(* let create_scene = Scenes.lens 300 *)
let create_scene = Scenes.globe 300
(* let create_scene = Scenes.room 300 *)
(* let create_scene = Scenes.three_spheres 300 *)
(* let create_scene = Scenes.onshape 300 *)

let params : Render.params = { samples_per_pixel = 4; max_depth = 10 }
let tracer = Whitted_tracer.whitted
(* let tracer = Random_walk_tracer.random_walk *)

let render n i pool =
  Printf.printf "Processing frame %d/%d" (i + 1) n;
  print_newline ();
  Render.create
    ~scene:(create_scene (float_of_int i /. float_of_int n))
    ~params ~tracer ~pool
  |> Ppm.of_render

let () =
  let seconds = 1.0 in
  let n = int_of_float (seconds *. 30.0) in
  let indices = List.init n Fun.id in
  let pool = T.setup_pool ~num_domains:5 () in
  List.iter
    (fun i ->
      Ppm.save
        ~filename:(Printf.sprintf "animation/frame_%05d.ppm" i)
        ~ppm:(render n i pool))
    indices;
  T.teardown_pool pool
