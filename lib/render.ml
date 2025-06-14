open Math
open Utils
module T = Domainslib.Task

type params = { samples_per_pixel : int; max_depth : int }
type tracer = scene:Scene.t -> ray:Ray.t -> max_depth:int -> Vec3.t

(* pixel color is stored as a sum of all samples and the number of samples *)
(* sum of all samples and the number of samples *)
type pixel_info = { weighted_color_sum : Vec3.t; sum_weights : float }

(* the full render *)
type t = { data : pixel_info array array; width : int; height : int }

let get_dim (render : t) = (render.width, render.height)
let empty width = { data = [||]; width; height = 0 }

let hstack r1 r2 =
  if r1.width <> r2.width then failwith "Renders must have the same width"
  else
    {
      data = Array.append r1.data r2.data;
      width = r1.width;
      height = r1.height + r2.height;
    }

let update_with_sample data ~col ~row color =
  let { weighted_color_sum; sum_weights } = data.(row).(col) in
  let new_info =
    {
      weighted_color_sum = weighted_color_sum +@ color;
      sum_weights = sum_weights +. 1.0;
    }
  in
  data.(row).(col) <- new_info

let create_section ~(scene : Scene.t) ~params ~tracer ~width ~start_row ~end_row
    =
  let height = end_row - start_row in
  let data =
    Array.make_matrix height width
      { weighted_color_sum = Vec3.zero (); sum_weights = 0.0 }
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      for _ = 1 to params.samples_per_pixel do
        let ray =
          Camera.get_ray ~camera:scene.camera ~col:x ~row:(start_row + y)
        in
        let color = tracer ~scene ~ray ~max_depth:params.max_depth in
        update_with_sample data ~col:x ~row:y color
      done
    done
  done;
  { data; width; height }

let create ~(scene : Scene.t) ~(params : params) ~(tracer : tracer)
    ~(pool : T.pool) : t =
  let width, height = Camera.get_pixel_dim scene.camera in
  let main _ =
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
              create_section ~scene ~params ~tracer ~width ~start_row:low
                ~end_row:high))
    in
    (* await and combine all sections *)
    List.fold_left
      (fun acc a -> hstack acc (T.await pool a))
      (empty width) sections
  in
  T.run pool main

let get_pixel_color ~(render : t) ~col ~row : Vec3.t =
  let pixel_val = render.data.(row).(col) in
  pixel_val.weighted_color_sum /@ pixel_val.sum_weights
  |> Vec3.max 0.0 |> Vec3.min 1.0
