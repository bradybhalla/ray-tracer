open Ray_tracer
open Ray_tracer.Math

type render_params = { samples_per_pixel : int }

module Render = struct
  (* pixel color is stored as a sum of all samples and the number of samples *)
  (* sum of all samples and the number of samples *)
  type pixel_info = { color_sum : Vec3.t; num : int }

  (* data in row major format *)
  type t = { data : pixel_info array array; width : int; height : int }

  let get_dim (render : t) = (`Col render.width, `Row render.height)

  let sample_at_pixel (camera : Camera.t) (scene : Object.t) (`Col col) (`Row row) =
    let ray = Camera.get_ray camera (`Col col) (`Row row) in
    let intersect = Object.ray_intersection ray scene in
    match intersect with
    | None -> Vec3.create 0. 0. 0.
    | Some _ -> Vec3.create 1. 0. 0.

  let update_with_new_sample data (`Col x) (`Row y) (c : Vec3.t) =
    let current_info = data.(y).(x) in
    let new_info =
      {
        color_sum = Vec3.(current_info.color_sum +@ c);
        num = current_info.num + 1;
      }
    in
    data.(y).(x) <- new_info

  let create (camera : Camera.t) (scene : Object.t) (params : render_params) : t
      =
    let `Col width, `Row height = Camera.get_output_dim camera in
    let data =
      Array.make_matrix height width
        { color_sum = Vec3.create 0. 0. 0.; num = 0 }
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        for _ = 1 to params.samples_per_pixel do
          let color = sample_at_pixel camera scene (`Col x) (`Row y) in
          update_with_new_sample data (`Col x) (`Row y) color
        done
      done
    done;
    { data; width; height }

  let get_pixel_color (render : t) (`Col x) (`Row y) : Vec3.t =
    let pixel_val = render.data.(y).(x) in
    Vec3.(pixel_val.color_sum /@ float_of_int pixel_val.num)
end

module PPM = Ppm.Make (Render)

let () =
  let camera =
    Camera.create ~fov:0.5 ~aspect_ratio:(16.0 /. 9.0) ~pixel_rows:30
  in
  let scene = Object.create ~pos:(Vec3.create 1.0 (-1.0) 10.0) ~radius:1.1 in
  let render_params = { samples_per_pixel = 100 } in
  Render.create camera scene render_params |> PPM.of_render |> PPM.print
