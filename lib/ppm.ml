type rgb = int * int * int

(* data in row major format *)
type t = {
  width : int;
  height : int;
  max_color_value : int;
  data : rgb array array;
}

let default_max_val = 255

let print (ppm : t) =
  Printf.printf "P3\n%d %d\n%d\n" ppm.width ppm.height ppm.max_color_value;
  Array.iter
    (fun row ->
      Array.iter (fun (r, g, b) -> Printf.printf "%d %d %d " r g b) row;
      print_newline ())
    ppm.data

let of_render (render : Render.t) =
  let `Col width, `Row height = Render.get_dim render in
  let data = Array.make_matrix height width (0, 0, 0) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let color = Render.get_pixel_color render (`Col x) (`Row y) in
      let r = int_of_float (color.x *. float_of_int default_max_val) in
      let g = int_of_float (color.y *. float_of_int default_max_val) in
      let b = int_of_float (color.z *. float_of_int default_max_val) in
      data.(y).(x) <- (r, g, b)
    done
  done;
  { width; height; max_color_value = default_max_val; data }
