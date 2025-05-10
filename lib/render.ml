open Math
open Utils
module T = Domainslib.Task

type scene = {
  camera : Camera.t;
  primitives : Primitive.t list;
  lights : Light.t list;
}

type params = { samples_per_pixel : int }
type tracer = scene -> Ray.t -> Vec3.t

(* pixel color is stored as a sum of all samples and the number of samples *)
(* sum of all samples and the number of samples *)
type pixel_info = { weighted_color_sum : Vec3.t; sum_weights : float }

(* the full render *)
type t = { data : pixel_info array array; width : int; height : int }

let get_dim (render : t) = (`Col render.width, `Row render.height)

let update_with_sample data (`Col x) (`Row y) (c : Vec3.t) =
  let { weighted_color_sum; sum_weights } = data.(y).(x) in
  let new_info =
    {
      weighted_color_sum = Vec3.copy weighted_color_sum |> Vec3.add c;
      sum_weights = sum_weights +. 1.0;
    }
  in
  data.(y).(x) <- new_info

(* TODO: probably setup and teardown pool in main function instead of here *)
let create ~(scene : scene) ~(params : params) ~(tracer : tracer) : t =
  let `Col width, `Row height = Camera.get_pixel_dim scene.camera in
  let data =
    Array.make_matrix height width
      { weighted_color_sum = Vec3.zero (); sum_weights = 0.0 }
  in
  let pool = T.setup_pool ~num_domains:4 () in
  let loop_size = height * width * params.samples_per_pixel in
  T.run pool (fun _ ->
      T.parallel_for pool ~start:0 ~finish:(loop_size - 1) ~body:(fun i ->
          let grid_i = i / params.samples_per_pixel in
          let x = grid_i mod width in
          let y = grid_i / width in
          let ray = Camera.get_ray scene.camera (`Col x) (`Row y) in
          let color = tracer scene ray in
          update_with_sample data (`Col x) (`Row y) color));
  T.teardown_pool pool;
  Gc.print_stat stderr;
  { data; width; height }

let get_pixel_color (render : t) (`Col x) (`Row y) : Vec3.t =
  let pixel_val = render.data.(y).(x) in
  pixel_val.weighted_color_sum /@ pixel_val.sum_weights
  |> Vec3.max 0.0 |> Vec3.min 1.0
