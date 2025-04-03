open Math

type params = { samples_per_pixel : int }
type tracer = Object.t -> Ray.t -> Vec3.t

(* pixel color is stored as a sum of all samples and the number of samples *)
(* sum of all samples and the number of samples *)
type pixel_info = { color_sum : Vec3.t; num : int }

(* data in row major format *)
type t = { data : pixel_info array array; width : int; height : int }

let get_dim (render : t) = (`Col render.width, `Row render.height)

let update_with_new_sample data (`Col x) (`Row y) (c : Vec3.t) =
  let current_info = data.(y).(x) in
  let new_info =
    {
      color_sum = Vec3.(current_info.color_sum +@ c);
      num = current_info.num + 1;
    }
  in
  data.(y).(x) <- new_info

let create ~(camera : Camera.t) ~(scene : Object.t) ~(params : params) ~(tracer : tracer) : t =
  let `Col width, `Row height = Camera.get_output_dim camera in
  let data =
    Array.make_matrix height width
      { color_sum = Vec3.create 0. 0. 0.; num = 0 }
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      for _ = 1 to params.samples_per_pixel do
        let ray = Camera.get_ray camera (`Col x) (`Row y) in
        let color = tracer scene ray in
        update_with_new_sample data (`Col x) (`Row y) color
      done
    done
  done;
  { data; width; height }

let get_pixel_color (render : t) (`Col x) (`Row y) : Vec3.t =
  let pixel_val = render.data.(y).(x) in
  Vec3.(pixel_val.color_sum /@ float_of_int pixel_val.num)
