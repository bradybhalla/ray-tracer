open Math
open Utils
module T = Domainslib.Task

type scene = {
  camera : Camera.t;
  primitives : Primitive.t list;
  lights : Light.t list;
}

type params = { samples_per_pixel : int; num_domains : int }

(* TODO: change back to not have col row camera after lod is fixed *)
type tracer = scene -> Ray.t -> [`Col of int] -> [`Row of int] -> Camera.t -> Vec3.t

(* pixel color is stored as a sum of all samples and the number of samples *)
(* sum of all samples and the number of samples *)
type pixel_info = { weighted_color_sum : Vec3.t; sum_weights : float }

(* the full render *)
type t = { data : pixel_info array array; width : int; height : int }

let get_dim (render : t) = (`Col render.width, `Row render.height)
let empty width = { data = [||]; width; height = 0 }

let hstack r1 r2 =
  if r1.width <> r2.width then failwith "Renders must have the same width"
  else
    {
      data = Array.append r1.data r2.data;
      width = r1.width;
      height = r1.height + r2.height;
    }

let update_with_sample data (`Col x) (`Row y) (c : Vec3.t) =
  let { weighted_color_sum; sum_weights } = data.(y).(x) in
  let new_info =
    {
      weighted_color_sum = weighted_color_sum +@ c;
      sum_weights = sum_weights +. 1.0;
    }
  in
  data.(y).(x) <- new_info

let create_section scene params (tracer: tracer) (`Col width) (`Row y0) (`Row y1) =
  let height = y1 - y0 in
  let data =
    Array.make_matrix height width
      { weighted_color_sum = Vec3.zero; sum_weights = 0.0 }
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      for _ = 1 to params.samples_per_pixel do
        let ray = Camera.get_ray scene.camera (`Col x) (`Row (y0 + y)) in
        let color = tracer scene ray (`Col x) (`Row (y0 + y)) scene.camera in
        update_with_sample data (`Col x) (`Row y) color
      done
    done
  done;
  { data; width; height }

(* TODO: probably setup and teardown pool in main function instead of here *)
let create ~(scene : scene) ~(params : params) ~(tracer : tracer) : t =
  let `Col width, `Row height = Camera.get_pixel_dim scene.camera in
  let pool = T.setup_pool ~num_domains:params.num_domains () in
  let res =
    T.run pool (fun _ ->
        (* split into sections ~20 pixels tall *)
        let num_sections = height / 20 in
        let bounds =
          Array.init (num_sections + 1) (fun i -> height * i / num_sections)
        in
        (* async create each section *)
        let sections =
          List.init num_sections (fun i ->
              let low = bounds.(i) in
              let high = bounds.(i + 1) in
              T.async pool (fun _ ->
                  create_section scene params tracer (`Col width) (`Row low)
                    (`Row high)))
        in
        (* await and combine all sections *)
        List.fold_left
          (fun acc a -> hstack acc (T.await pool a))
          (empty width) sections)
  in
  T.teardown_pool pool;
  res

let get_pixel_color (render : t) (`Col x) (`Row y) : Vec3.t =
  let pixel_val = render.data.(y).(x) in
  pixel_val.weighted_color_sum /@ pixel_val.sum_weights
  |> Vec3.max 0.0 |> Vec3.min 1.0
