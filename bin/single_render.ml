open Ray_tracer
module T = Domainslib.Task

let scene = Scenes.materials 300 0.0
(* let scene = Scenes.lens 300 0.0 *)
(* let scene = Scenes.globe 800 0.9 *)
(* let scene = Scenes.room 300 0.9 *)
(* let scene = Scenes.three_spheres 300 0.0 *)
(* let scene = Scenes.onshape 300 0.1 *)

let params : Render.params = { samples_per_pixel = 100; max_depth = 10 }

let tracer = Whitted_tracer.whitted
(* let tracer = Random_walk_tracer.random_walk *)

let () =
  let pool = T.setup_pool ~num_domains:5 () in
  Render.create ~scene ~params ~tracer ~pool |> Ppm.of_render |> Ppm.print;
  T.teardown_pool pool
