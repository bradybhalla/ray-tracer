type rgb = int * int * int

(* data in row major format *)
type t = {
  width : int;
  height : int;
  max_color_value : int;
  data : rgb array array;
}

let default_max_val = 255
let gamma_correction = 1.0 /. 2.2

let to_string (ppm : t) =
  let header =
    Printf.sprintf "P3\n%d %d\n%d\n" ppm.width ppm.height ppm.max_color_value
  in
  let pixel_strings =
    Array.map
      (fun row ->
        Array.map (fun (r, g, b) -> Printf.sprintf "%d %d %d " r g b) row)
      ppm.data
  in
  Array.fold_left
    (fun acc r ->
      let row_string = Array.fold_left ( ^ ) "" r in
      acc ^ row_string ^ "\n")
    header pixel_strings

let print (ppm : t) = to_string ppm |> print_string
let save (filename : string) (ppm : t)  = 
  let oc = open_out filename in
  to_string ppm |> output_string oc;
  close_out oc

let of_render (render : Render.t) =
  let `Col width, `Row height = Render.get_dim render in
  let data = Array.make_matrix height width (0, 0, 0) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let color = Render.get_pixel_color render (`Col x) (`Row y) in
      let r =
        int_of_float
          (Float.pow color.x gamma_correction *. float_of_int default_max_val)
      in
      let g =
        int_of_float
          (Float.pow color.y gamma_correction *. float_of_int default_max_val)
      in
      let b =
        int_of_float
          (Float.pow color.z gamma_correction *. float_of_int default_max_val)
      in
      data.(y).(x) <- (r, g, b)
    done
  done;
  { width; height; max_color_value = default_max_val; data }
